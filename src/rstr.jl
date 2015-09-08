
function rscript(script)
    sf = protect(rcall_p(:srcfile,"xx"))
    status = Array(Cint,1)
    k = 0
    rsyms = ASCIIString[]    
    exprs = Any[]
    jsymdict = Dict{Symbol,ASCIIString}()
    local parsedata

    try
        while true
            # attempt to parse string
            ret = ccall((:R_ParseVector,libR),UnknownSxpPtr,
                        (Ptr{StrSxp},Cint,Ptr{Cint},Ptr{EnvSxp}),
                        sexp(script),-1,status,sf)
            if status[1] == 2
                error("RCall.jl incomplete R expression")
            end
            
            parsedata = protect(rcall_p(:getParseData,sf))
            n = length(parsedata[1])
            
            lineno = parsedata[1][n]
            charno = parsedata[2][n] # this is the character no., not byte numbe
            # note: unicode character numbers are incorrect, see https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16524
            c = rcopy(UTF8String,parsedata[9][n])
            unprotect(1)

            if status[1] == 1
                break # valid R string
            end


            if  c != "\$"
                error("RCall.jl: invalid R expression")
            end

            # skip to string location
            i = start(script)
            for j = 1:lineno-1
                i = search(script,'\n',i)
                i = nextind(script,i)
            end
            for j = 1:charno-1
                i = nextind(script,i)
            end
            
            # now script[i] == '\$'
            # assuming no unicode, see
            # https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16524
            i_stop = prevind(script,i)
            c,i = next(script,i)            
            c == '\$' || error("RCall.jl: incorrect R parsing")

            

            expr,i = parse(script,i,greedy=false)
            if isa(expr,Symbol)
                
            end
            push!(exprs,expr)

            

            k += 1
            sym = "##RCall##$k"            
            push!(rsyms,sym)

            
            script = string(script[1:i_stop],'`',sym,'`',script[i:end])
        end
    finally
        unprotect(1)        
    end
    return script, RObject(parsedata), rsyms, exprs
end
