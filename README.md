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
@VFun(MyFuncName2, Int, Int)

#The function can be called like a struct now
test(func::MyFuncName{Int})
       return func(1, 2)
end

#Wrap the anonymous function
test(MyFuncName{Int}((x1, x2) -> x1 * x2)

#Have it guess the types (careful with this though, better to manually type)
test(MyFuncName((x1, x2) -> Int(x1 * x2))

#string(MyFuncName{Int}) will return ".MyFuncName{Int}(arg1::Int, arg2::int):(return_arg::Int)"
#Docs will also display this aswell
```