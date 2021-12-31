module FunWrap

include("Iterable.jl")

export Fun, VFun, RFun, NFun, NVFun, NRFun, EFun, NEFun,
             @Fun, @VFun, @RFun, @NFun, @NVFun, @NRFun, @EFun, @NEFun, FunDef, @CFun


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
        end
        Fun(r, a, _function, Symbol(_function))
    end
    
    Base.string(f::Fun) = string(typeof(f))
    Base.isequal(f::Fun, f2::Fun) = (typeof(f) == typeof(f2)) && f._function == f2._function
    Base.show(io::IO, f::Fun) = print(io, string(f))
    Base.print(io::IO, f::Fun) = print(io, string(f))
    Docs.getdoc(f::Fun) = string(f)

    checkeq(::Type{Fun{R, A, H}}, ::Type{Fun{R2, A2, H2}}) where {R, A, H, R2, A2, H2} = (R == R2 && A == A2 && H == H2)
    Base.isequal(t::Type{F1}, t2::Type{F2}) where {F1 <: Fun, F2 <: Fun} = checkeq(t, t2)
    Base.print(io::IO, f::Type{F}) where F <: Fun = print(io, string(f))
    Base.show(io::IO, f::Type{F}) where F <: Fun = show(io, string(f))   
    Base.string(f::Type{Fun{R, A, H}}) where {R, A, H} = "Fun{$R, $A}"  
    Docs.getdoc(f::Type{F}) where F <: Fun = string(f)
    
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

fun_str(msg) = replace(msg, "~" => "")


"Default Function TypeDef Generator"
function FunDef(mod, line, fun, ret, args, msg)
    partialTypeDef = nothing
    partialStringDef = nothing
    partialCallDef = nothing
    partialStringDef2 = nothing

    try
        #Strip Return Label
        ret = (ret isa Expr && ret.head == :(::)) ? ret.args[2] : ret
        argtypes = nothing
        argnames = nothing
        args = Any[arg for arg in args]
        aliasName = fun isa Expr ? fun.args[1] : fun
        generics = fun isa Expr ? fun.args[2:end] : []

        if args isa Expr || args isa Symbol
            #Strip Arg Labels
            if args isa Expr && args.head == :(::)
                argtypes = [args.args[2]]
                argnames = [args.args[1]]
            else
                #Add Argument Label
                argnames = [:arg1]
                argtypes = [args]
                args = Expr(:(::), :arg1, args)
            end
        else
            argtypes = Any[arg for arg in args]
            argnames = [Symbol("arg$i") for i in 1:length(args)]
            for i in 1:length(args)
                if argtypes[i] isa Expr && args[i].head == :(::)
                    argnames[i] = argtypes[i].args[1]
                    argtypes[i] = argtypes[i].args[2]
                else
                    args[i] = Expr(:(::), argnames[i], args[i])
                end
            end 
            argtypes = Expr(:curly, :Tuple, argtypes...)
        end



        #Create Alias (Use Hash/Rand for Equality Checking)
        curlyBlock = Expr(:curly, GlobalRef(@__MODULE__, :Fun), ret, argtypes, msg === nothing ? rand() : hash(msg))
        partialTypeDef = Expr(:const, Expr(:(=), fun, curlyBlock))

        aliasResult = Core.eval(mod, partialTypeDef)
        


        #Define most of the specialized function call expr
        partialCallDef = :(@inline function (f::$fun)()::$ret where {}
                                        f._function()
                                   end)

        partialCallDef.args[3].args[2].args[1] = LineNumberNode(line.line, line.file)
        partialCallDef.args[3].args[2].args[2] = LineNumberNode(line.line, line.file)

        #Append Function Generics
        whereExpr = partialCallDef.args[3].args[1].args
        append!(whereExpr, generics)    
        
        #Append the arguments to the function Call
        funArgs = partialCallDef.args[3].args[1].args[1].args[1].args
        append!(funArgs, args)

        #Append the arguments to the inner function 
        funBlock = partialCallDef.args[3].args[2].args[3].args
        append!(funBlock, argnames)

        Core.eval(mod, partialCallDef)
        msg === nothing && return aliasResult




        #Eval No Generic Message
        if length(generics) > 0
            #Define most of the function string expr
            partialStringDef2 = :(Base.string(x::Type{$aliasName}) = fun_str($msg))
           
            #Replace with safer reference
            fun_str_call = partialStringDef2.args[2].args[2].args
            fun_str_call[1] = GlobalRef(@__MODULE__, :fun_str)

            Core.eval(mod, partialStringDef2)
        end




        #Define most of the function string expr
        partialStringDef = :(Base.string(x::Type{$fun}) where {} = fun_str($aliasName, $msg))

        #Append Function Generics
        whereExpr = partialStringDef.args[1]
        append!(whereExpr.args, generics)

        fun_call = partialStringDef.args[2].args[2].args
        #Replace with safer reference
        fun_call[1] = GlobalRef(@__MODULE__, :fun_str)

        #Create Tuple instead of curly to pass to function
        tupleExpr = Expr(:tuple)
        append!(tupleExpr.args, generics)
        push!(fun_call, tupleExpr)

        #Set Line Information
        partialStringDef.args[2].args[1] = LineNumberNode(line.line, line.file) 
        Core.eval(mod, partialStringDef)
        return aliasResult
    catch e
        println("Error in Function Generating $fun in $mod")
        println("PartialTypeDef:$partialTypeDef")
        println("PartialCallDef:$partialCallDef")
        println("PartialStringDef2:$partialStringDef")
        println("PartialStringDef:$partialStringDef")
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
end # module
