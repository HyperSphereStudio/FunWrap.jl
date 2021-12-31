Wrapper Library designed to wrap function objects to ensure the correct signature. Very similar to using typedef in C++.

To Install:
```julia
#To Use
Pkg.add("FunWrap")

using FunWrap
```

Usage:
```julia
#Create the wrapper you want
@Fun(MyFuncName{T}, return_arg::T, arg1::Int, arg2::Int)

#Void version and no var names
@VFun(MyFuncName2{T}, Int, Int)

test(func::MyFuncName{Int})
    return func(1, 2)
end

test(MyFuncName{Int}((x1, x2) → x1 * x2)

#Have it guess the types (careful with this though, better to manually type)
test(MyFuncName((x1, x2) → Int(x1 * x2))
```

This library will have the function string(MyFuncName{Int}) return “.MyFuncName{Int}(arg1::Int, arg2::int):(return_arg::Int)”

TODO:
1. Make calls using the function wrapper inlined
2. Types are truly enforced
3. Make docs show the function string