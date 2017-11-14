using ParallelAccelerator

@acc function fdtd(tmax::Int, nx::Int, ny::Int, nxm1::Int, nym1::Int,
                   ex::Matrix{Float64}, ey::Matrix{Float64},
                   hz::Matrix{Float64}, _fict_::Vector{Float64})

     for t = 1:tmax
         for j = 1:ny
             ey[1, j] = _fict_[j]
         end
         for i = 2:nx
             for j = 1:ny
                 ey[i, j] -= 0.5 * (hz[i, j] - hz[i - 1, j])
             end
         end
         for i = 1:nx
             for j = 1:ny
                 ex[i, j] -= 0.5 * (hz[i, j] - hz[i, j - 1])
             end
         end
         for i = 1:nxm1
             for j = 1:nym1
                 hz[i, j] -= 0.7 * (ex[i, j + 1] - ex[i, j] + ex[i + 1, j] - ey[i, j])
             end
         end
     end
end

tmax = 50
nx = 50
ny = nx

nxm1 = nx - 1
nym1 = ny - 1

_fict_ = rand(tmax)
ex1 = rand(nx, ny)
ex2 = copy(ex1)
ey1 = rand(nx, ny)
ey2 = copy(ey1)
hz1 = rand(nx, ny)
hz2 = copy(hz1)

fdtd(tmax, nx, ny, nxm1, nym1, ex1, ey1, hz1, _fict_)
