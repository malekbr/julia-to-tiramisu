using ParallelAccelerator

@acc function bicg(m::Int, n::Int, A::Matrix{Float64},
     s::Vector{Float64}, q::Vector{Float64}, p::Vector{Float64}, r::Vector{Float64})

     for i = 1:m
          s[i] = 0.0
     end
     for i = 1:n
          q[i] = 0.0
          for j = 1:m
               s[j] += r[i] * A[i, j]
               q[i] += A[i, j] * p[j]
          end
     end
end

@inbounds function bicg_o(m::Int, n::Int, A::Matrix{Float64},
     s::Vector{Float64}, q::Vector{Float64}, p::Vector{Float64}, r::Vector{Float64})

     tic()
     for i = 1:m
          @fuse 1 s[i] = 0.0
     end
     for i = 1:n
          q[i] = 0.0
          for j = 1:m
               s[j] += r[i] * A[i, j]
               q[i] += A[i, j] * p[j]
          end
     end
     println("Julia $(toq())")
end

m = 20000
n = m

A = rand(n, m)
st = Vector{Float64}(m)
qt = Vector{Float64}(n)
p = rand(m)
r = rand(n)

bicg(m, n, A, st, qt, p, r)

s = Vector{Float64}(m)
q = Vector{Float64}(n)

bicg_o(m, n, A, s, q, p, r)


function compare(A, B)
    for i in 1:n
        if A[i] != B[i]
            println("different $((i)) : $(A[i]) != $(B[i])")
            return false
        end
    end
    return true
end

println(compare(s, st))
println(compare(q, qt))
