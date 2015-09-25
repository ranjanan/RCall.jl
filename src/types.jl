"""
R symbolic expression (`SxpPtr`): these are represented by a pointer to a
symbolic expression record (`Sxp`).
"""
abstract Sxp # SEXPREC
typealias SxpPtr{S<:Sxp} Ptr{S} # SEXP
typealias SxpPtrInfo UInt32 # sxpinfo_struct


"""
R Sxp header: a pointer to this is used for unknown types.
"""
immutable SxpHead <: Sxp # SEXPREC_HEADER
    info::SxpPtrInfo
    attrib::Ptr{SxpHead}
    gc_next::Ptr{SxpHead}
    gc_prev::Ptr{SxpHead}
end
typealias UnknownSxpPtr Ptr{SxpHead}

abstract VectorSxp <: Sxp
abstract VectorAtomicSxp <: VectorSxp
abstract VectorNumericSxp <: VectorAtomicSxp
abstract VectorListSxp <: VectorSxp
abstract PairListSxp <: Sxp
abstract FunctionSxp <: Sxp


"""
R NULL value - type tag 0
"""
immutable NilSxp <: PairListSxp
    head::SxpHead
end
typealias NilSxpPtr Ptr{NilSxp}

"""
R pairs (cons) list cell - type tag 2
"""
immutable ListSxp <: PairListSxp
    head::SxpHead
    car::UnknownSxpPtr
    cdr::UnknownSxpPtr
    tag::UnknownSxpPtr
end
typealias ListSxpPtr Ptr{ListSxp}

"""
R function closure - type tag 3
"""
immutable ClosSxp <: FunctionSxp
    head::SxpHead
    formals::ListSxpPtr
    body::UnknownSxpPtr
    env::UnknownSxpPtr
end
typealias ClosSxpPtr Ptr{ClosSxp}

"""
R environment - type tag 4
"""
immutable EnvSxp <: Sxp
    head::SxpHead
    frame::UnknownSxpPtr
    enclos::UnknownSxpPtr
    hashtab::UnknownSxpPtr
end
typealias EnvSxpPtr Ptr{EnvSxp}

"""
R promise - type tag 5
"""
immutable PromSxp <: Sxp
    head::SxpHead
    value::UnknownSxpPtr
    expr::UnknownSxpPtr
    env::UnknownSxpPtr
end
typealias PromSxpPtr Ptr{PromSxp}

"""
R function call - type tag 6
"""
immutable LangSxp <: PairListSxp
    head::SxpHead
    car::UnknownSxpPtr
    cdr::UnknownSxpPtr
    tag::UnknownSxpPtr
end
typealias LangSxpPtr Ptr{LangSxp}

"""
R special function - type tag 7
"""
immutable SpecialSxp <: FunctionSxp
    head::SxpHead
end
typealias SpecialSxpPtr Ptr{SpecialSxp}

"""
R built-in function - type tag 8
"""
immutable BuiltinSxp <: FunctionSxp
    head::SxpHead
end
typealias BuiltinSxpPtr Ptr{BuiltinSxp}

"""
R character string - type tag 9

This type is never visible in the R REPL
"""
immutable CharSxp <: VectorAtomicSxp
    head::SxpHead
    length::Cint
    truelength::Cint
end
typealias CharSxpPtr Ptr{CharSxp}

"""
R symbol - type tag 1
"""
immutable SymSxp <: Sxp
    head::SxpHead
    name::CharSxpPtr
    value::UnknownSxpPtr
    internal::UnknownSxpPtr
end
typealias SymSxpPtr Ptr{SymSxp}

"""
R logical vector - type tag 10
"""
immutable LglSxp <: VectorNumericSxp
    head::SxpHead
    length::Cint
    truelength::Cint
end
typealias LglSxpPtr Ptr{LglSxp}

"""
R integer vector - type tag 13
"""
immutable IntSxp <: VectorNumericSxp
    head::SxpHead
    length::Cint
    truelength::Cint
end
typealias IntSxpPtr Ptr{IntSxp}

"""
R real vector - type tag 14
"""
immutable RealSxp <: VectorNumericSxp
    head::SxpHead
    length::Cint
    truelength::Cint
end
typealias RealSxpPtr Ptr{RealSxp}

"""
R complex vector - type tag 15
"""
immutable CplxSxp <: VectorNumericSxp
    head::SxpHead
    length::Cint
    truelength::Cint
end
typealias CplxSxpPtr Ptr{CplxSxp}

"""
R vector of character strings - type tag 16
"""
immutable StrSxp <: VectorListSxp
    head::SxpHead
    length::Cint
    truelength::Cint
end
typealias StrSxpPtr Ptr{StrSxp}

"""
R dot-dot-dot object - type tag 17
"""
immutable DotSxp <: Sxp
    head::SxpHead
end
typealias DotSxpPtr Ptr{DotSxp}

"""
R \"any\" object - type tag 18
"""
immutable AnySxp <: Sxp
    head::SxpHead
end
typealias AnySxpPtr Ptr{AnySxp}

"""
R list (i.e. Array{Any,1}) - type tag 19
"""
immutable VecSxp <: VectorListSxp
    head::SxpHead
    length::Cint
    truelength::Cint
end
typealias VecSxpPtr Ptr{VecSxp}

"""
R expression vector - type tag 20
"""
immutable ExprSxp <: VectorListSxp
    head::SxpHead
    length::Cint
    truelength::Cint
end
typealias ExprSxpPtr Ptr{ExprSxp}

"""
R byte code - type tag 21
"""
immutable BcodeSxp <: Sxp
    head::SxpHead
end
typealias BcodeSxpPtr Ptr{BcodeSxp}

"""
R external pointer - type tag 22
"""
immutable ExtPtrSxp <: Sxp
    head::SxpHead
    ptr::Ptr{Void}
    prot::Ptr{Void}
    tag::UnknownSxpPtr
end
typealias ExtPtrSxpPtr Ptr{ExtPtrSxp}

"""
R weak reference - type tag 23
"""
immutable WeakRefSxp <: Sxp
    head::SxpHead
end
typealias WeakRefSxpPtr Ptr{WeakRefSxp}

"""
R byte vector - type tag 24
"""
immutable RawSxp <: VectorAtomicSxp
    head::SxpHead
    length::Cint
    truelength::Cint
end
typealias RawSxpPtr Ptr{RawSxp}

"""
R S4 object - type tag 25
"""
immutable S4Sxp <: Sxp
    head::SxpHead
end
typealias S4SxpPtr Ptr{S4Sxp}


typealias VectorSxpPtr{S<:VectorSxp} Ptr{S}
typealias VectorAtomicSxpPtr{S<:VectorAtomicSxp} Ptr{S}
typealias VectorNumericSxpPtr{S<:VectorNumericSxp} Ptr{S}
typealias VectorListSxpPtr{S<:VectorListSxp} Ptr{S}
typealias PairListSxpPtr{S<:PairListSxp} Ptr{S}
typealias FunctionSxpPtr{S<:FunctionSxp} Ptr{S}


"""
Element types of R vectors.
"""
eltype(::Type{LglSxp}) = Cint
eltype(::Type{IntSxp}) = Cint
eltype(::Type{RealSxp}) = Float64
eltype(::Type{CplxSxp}) = Complex128
eltype(::Type{CharSxp}) = UInt8
eltype(::Type{RawSxp}) = UInt8

eltype(::Type{StrSxp}) = Ptr{CharSxp}
eltype(::Type{VecSxp}) = UnknownSxpPtr
eltype(::Type{ExprSxp}) = UnknownSxpPtr





"""
An `RObject` is a Julia wrapper for an R object (known as an "S-expression" or "SEXP"). It is stored as a pointer which is protected from the R garbage collector, until the `RObject` itself is finalized by Julia. The parameter is the type of the S-expression.

When called with a Julia object as an argument, a corresponding R object is constructed.

```julia
julia> RObject(1)
RObject{IntSxp}
[1] 1

julia> RObject(1:3)
RObject{IntSxp}
[1] 1 2 3

julia> RObject(1.0:3.0)
RObject{RealSxp}
[1] 1 2 3
```
"""
type RObject{S<:Sxp}
    p::Ptr{S}
    function RObject(p::Ptr{S})
        preserve(p)
        r = new(p)
        finalizer(r, release)
        r
    end
    # SymSxps are not garbage collected, so preserve not necessary.
    RObject(p::Ptr{SymSxp}) = new(p)
end
RObject{S<:Sxp}(p::Ptr{S}) = RObject{S}(p)
RObject(x::RObject) = x
RObject(x) = RObject(sexp(x))


# convert{T}(::Type{T}, r::RObject) = convert(T,r.p)

"""
Prevent garbage collection of an R object. Object can be released via `release`.

This is slower than `protect`, as it requires searching an internal list, but
more flexible.
"""
preserve{S<:Sxp}(p::Ptr{S}) = ccall((:R_PreserveObject,libR), Void, (Ptr{S},), p)

"""
Release object that has been gc-protected by `preserve`.
"""
release{S<:Sxp}(p::Ptr{S}) = ccall((:R_ReleaseObject,libR),Void,(Ptr{S},),p)
release{S<:Sxp}(r::RObject{S}) = release(r.p)

"""
Stack-based protection of garbage collection of R objects. Objects are
released via `unprotect`. Returns the same pointer, allowing inline use.

This is faster than `preserve`, but more restrictive. Really only useful
inside functions.
"""
protect{S<:Sxp}(p::Ptr{S}) = ccall((:Rf_protect,libR), Ptr{S}, (Ptr{S},), p)

"""
Release last `n` objects gc-protected by `protect`.
"""
unprotect(n::Integer) = ccall((:Rf_unprotect,libR), Void, (Cint,), n)

"""
The SEXPTYPE number of a `Sxp`

Determined from the trailing 5 bits of the first 32-bit word. Is
a 0-based index into the `info` field of a `SxpHead`.
"""
sexpnum(h::SxpHead) = h.info & 0x1f
sexpnum(p::SxpPtr) = sexpnum(unsafe_load(p))

"""
Vector of R Sxp types
"""
const typs = [NilSxp,SymSxp,ListSxp,ClosSxp,EnvSxp,
              PromSxp,LangSxp,SpecialSxp,BuiltinSxp,CharSxp,
              LglSxp,Void,Void,IntSxp,RealSxp,
              CplxSxp,StrSxp,DotSxp,AnySxp,VecSxp,
              ExprSxp,BcodeSxp,ExtPtrSxp,WeakRefSxp,RawSxp,
              S4Sxp]

for (i,T) in enumerate(typs)
    if T != Void
        @eval sexpnum(::Type{$T}) = $(i-1)
    end
end


"""
Convert a `UnknownSxpPtr` to an approptiate `SxpPtr`.
"""
function sexp(p::UnknownSxpPtr)
    typ = sexpnum(p)
    0 ≤ typ ≤ 10 || 13 ≤ typ ≤ 25 || error("Unknown SEXPTYPE $typ")
    styp = typs[typ+1]
    convert(Ptr{styp},p)
end
sexp(s::SxpPtr) = s
sexp(r::RObject) = r.p

sexp{S<:Sxp}(::Type{S},s::Ptr{S}) = s
sexp{S<:Sxp}(::Type{S},r::RObject{S}) = r.p
