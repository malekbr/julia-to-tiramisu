using ParallelAccelerator

@acc function kernel_doitgen(nr::Int, nq::Int, np::Int,
    A::Array{Float64, 3}, C4::Matrix{Float64})

    sum = Array{Float64}(np)
    for r = 1:nr
        for q = 1:nq
            for p = 1:np
                sum[p] = 0.0
                for s = 1:np
                    sum[p] += A[r, q, s] * C4[s, p]
                end
            end
            for p = 1:np
                A[r, q, p] = sum[p]
            end
        end
    end
end

@inbounds function kernel_doitgen_o(nr::Int, nq::Int, np::Int,
    A::Array{Float64, 3}, C4::Matrix{Float64})

    tic()
    sum = Array{Float64}(np)
    for r = 1:nr
        for q = 1:nq
            for p = 1:np
                sum[p] = 0.0
                for s = 1:np
                    sum[p] += A[r, q, s] * C4[s, p]
                end
            end
            for p = 1:np
                A[r, q, p] = sum[p]
            end
        end
    end
    println("Julia $(toq())")
end

nr = 300
nq = nr
np = nr

At = rand(nr, nq, np)
C4 = rand(np, np)

Ao = copy(At)

kernel_doitgen(nr, nq, np, At, C4)
kernel_doitgen_o(nr, nq, np, Ao, C4)

function compare(A, B)
    for i in 1:nr
        for j in 1:nq
            for k in 1:np
                if A[i, j, k] != B[i, j, k]
                    println("different $((i, j, k)) : $(A[i, j, k]) != $(B[i, j, k])")
                    return false
                end
            end
        end
    end
    return true
end

println(compare(At, Ao))
