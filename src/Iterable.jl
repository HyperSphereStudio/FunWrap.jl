#Written By Johnathan Bizzano
export Iterable

struct Iterable{ElType}
    backend
    
    function Iterable(backend)
        ElType = Base.eltype(backend)
        new{ElType}(backend)
    end

    Base.eltype(i::Iterable) = Base.eltype(i.backend)
    Base.length(i::Iterable) = Base.length(i.backend)
    Base.elsize(i::Iterable) = Base.elsize(i.backend)
    Base.eltypeof(i::Iterable) = Base.eltypeof(i.backend)
    Base.iterate(i::Iterable) = Base.iterate(i.backend)
    Base.iterate(i::Iterable, state) = Base.iterate(i.backend, state)
    Base.size(i::Iterable, dim) = Base.size(i.backend, dim)
    Base.convert(::Iterable, x) = Iterable(x)
end


