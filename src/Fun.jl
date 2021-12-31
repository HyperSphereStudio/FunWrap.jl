#Written By Johnathan Bizzano
module FunJL

include("Iterable.jl")

export Fun, VFun, RFun, NFun, NVFun, NRFun, EFun, NEFun,
             @Fun, @VFun, @RFun, @NFun, @NVFun, @NRFun, @EFun, @NEFun, FunDef, @DocFun, @CFun

"Hashed Function (Args & Return)"
struct Fun{R, A, H}
    _function::Function

    "Pass Through"
    Fun(_function::Fun) = _function
    Fun{R, A, H}(_function::Fun) where {R, A, H} = Fun{R, A, H} == typeof(_function) ? _function : new{R, A, H}(_function._function)
    Fun(R, A, H, _function::Function) = new{R, A, H}(_function)
    Fun{R, A, H}(_function::Function) where {R, A, H} = Fun(R, A, H, _function)
    
    function Fun(_function::Function)
        r = Base.return_types(_function)[1]
        a = methods(_function)[1].sig.parameters[2:end]
        if length(a) > 1
            a = Tuple{a...,}
        else
            a = a[1]
        end
        Fun(r, a, _function, Symbol(_function))
    end

    (f::Fun{R, A, H})(@specialize(args...)) where {R, A, H} = f._function(args...)


    
    Base.string(f::Fun) = string(typeof(f))
    Base.isequal(f::Fun, f2::Fun) = (typeof(f) == typeof(f2)) && f._function == f2._function
    Base.show(io::IO, f::Fun) = print(io, string(f))
    Base.print(io::IO, f::Fun) = print(io, string(f))

    Base.isequal(::Type{Fun{R, A, H}}, ::Type{Fun{R2, A2, H2}}) where {R, A, H, R2, A2, H2} = (R == R2 && A == A2 && H == H2)
    Base.print(io::IO, f::Type{Fun}) = print(io, string(f))
    Base.show(io::IO, f::Type{Fun}) = show(io, string(f))     
    Base.string(::Type{Fun{R, A, H}}) where {R, A, H} = "Fun{$R, $A}" 
end

function fun_str(alias, msg, generic_values)
    idx = 1
    while alias isa UnionAll
        msg = replace(msg, (string(alias.var.name) * "~") => generic_values[idx])
        alias = alias.body
        idx += 1
    end
    replace(msg, "~" => "")
end


"Default Function TypeDef Generator"
function FunDef(mod, line, expr, ret, args, msg)
    result = nothing
    partialStringDef = nothing
    try
        #Strip Return Label
        ret = (ret isa Expr && ret.head == :(::)) ? ret.args[2] : ret
        args = args
        aliasName = expr isa Expr ? expr.args[1] : expr
        generics = expr isa Expr ? expr.args[2:end] : []

        if args isa Expr
            #Strip Arg Labels
            (args.head == :(::)) && (args = args.args[2])
        else
            #Strip Args Labels
            if !(args isa Symbol)
                Args = Any[arg for arg in args]
                for i in 1:length(args)
                    if Args[i] isa Expr && Args[i].head == :(::)
                        Args[i] = Args[i].args[2]
                    end
                end 

                #Create Arg Tuple
                args = Expr(:curly, :Tuple, Args...)
            end
        end

        #Create Alias (Use Hash/Rand for Equality Checking)
        curlyBlock = Expr(:curly, GlobalRef(@__MODULE__, :Fun), ret, msg === nothing ? rand() : hash(msg), args)
        result = Expr(:const, Expr(:(=), expr, curlyBlock))

        aliasResult = Core.eval(mod, result)
        
        msg === nothing && return aliasResult

        partialStringDef = :(Base.string(x::Empty) where {} = fun_str($aliasName, $msg))

        whereExpr = partialStringDef.args[1]
        #Append Function Generics
        append!(whereExpr.args, generics)

        #Replace Empty with Alias
        itemLine = whereExpr.args[1].args[2].args
        itemLine[2] = expr

        fun_call = partialStringDef.args[2].args[2].args
        #Replace with safer reference
        fun_call[1] = GlobalRef(@__MODULE__, :fun_str)

        #Create Tuple instead of curly to pass to function
        tupleExpr = Expr(:tuple)
        append!(tupleExpr.args, generics)
        push!(fun_call, tupleExpr)

        #Set Line Information
        partialStringDef.args[2].args[1] = LineNumberNode(line.line + 1, line.file)   

        Core.eval(mod, partialStringDef)
        return aliasResult
    catch e
        println("Error in Function Generating $expr in $mod")
        println(result)
        println(partialStringDef)
        throw(e)
    end
end

function deffhash(mod, expr, ret, args) 
    str = "$mod.$expr("
    startswith(str, "Main.") && (str = SubString(str, 6, length(str)))
    if args != :Nothing
        str *= join(args, "~, ") * "~"
    end

    (ret == :Nothing) && return str * ")"
    return string(str * ")::($ret~)")
end

"Document a Fun Gen"
macro DocFun(funcType, str)
    type = Expr(:(::), :x, funcType isa Expr ? funcType.args[1] : funcType)
    Core.eval(__module__, :(Docs.getdoc($type) = $str))
end

"Default Message Function (Args & Return) Type Definition Generator"
macro Fun(expr, ret, args...)
    FunDef(__module__, __source__,  expr, ret, args, deffhash(__module__,expr, ret, args))
end

"Custom FuncType And Additional Function (Args & Return) Type Definition Generator"
macro CFun(expr, ret, msg, args...)
    FunDef(__module__, __source__,  expr, ret, args, deffhash(msg, expr, ret, args))
end


"Void Message Function (Args Only)"
const VFun{A} = Fun{Nothing, A}

"Default Void Message Function (Args Only) Type Definition Generator"
macro VFun(expr, args...)
    return FunDef(__module__, __source__, expr, :Nothing, args, deffhash(__module__, expr, :Nothing, args))
end


"Void Message Function"
const EFun{MSG} = Fun{Nothing, Nothing}

"Void Default Message Function Type Definition Generator"
macro EFun(expr)
    return FunDef(__module__, __source__, expr, :Nothing, :Nothing, deffhash(__module__,expr, :Nothing, args))
end


"Default Return Message Function (Return Only)"
const RFun{R} = Fun{R, Nothing}

"Default Return Message Function (Return Only) Type Definition Generator"
macro RFun(expr, ret)
    return FunDef(__module__, __source__, expr, ret, :Nothing, deffhash(__module__,expr, ret, :Nothing))
end



"No Message Function (Args & Return)"
const NFun{R, A} = Fun{R, A, Nothing}

"No Message Function (Args & Return) Type Definition Generator"
macro NFun(expr, ret, args...)
    return FunDef(__module__, __source__, expr, ret, args, nothing)
end



"No Message Void Function (Args only)"
const NVFun{A} = Fun{Nothing, A, Nothing}

"No Message Void Function (Args only) Type Definition Generator"
macro NFun(expr, args...)
    FunDef(__module__, __source__, expr, :Nothing, args, nothing)
end


"No Message Return Function (Return only)"
const NRFun{R} = Fun{R, Nothing, Nothing}

"No Message Return Function (Return only) Type Definition Generator"

macro NFun(expr, ret,  args...)
    FunDef(__module__, __source__, expr, ret, :Nothing, nothing)
end

"Void No Message Function"
const NEFun = Fun{Nothing, Nothing, Nothing}

"Void No Message Function Type Definition Generator"
macro NEFun(expr)
    FunDef(__module__, __source__, expr, :Nothing, :Nothing, nothing)
end
end