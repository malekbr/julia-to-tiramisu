using ParallelAccelerator

@acc function loopIt()
  A = Array{Int8}(10000,
                  10000)
  B = Array{Int8}(10000, 10000)
  C = Array{Int8}(10000, 10000)
  D = Array{Int8}(10000, 10000)
  for i = 1:10000
    for j = 1:10000
      @fuse 2 A[i, j] = Int8(0)
    end
  end
  for i = 1:10000
    for j = 1:10000
      @fuse 1 B[i, j] = A[i, j] + Int8(1)
    end
  end
  for i = 1:10000
    for j = 1:10000
      @fuse 1 C[i, j] = B[i, j] + Int8(2)
    end
  end
  for i = 1:10000
    for j = 1:10000
      D[i, j] = C[i, j] + Int8(3)
    end
  end
  return D
end

# @acc function loopIt()
#   A = Array{Int8}(10000, 10000)
#   B = Array{Int8}(10000, 10000)
#   C = Array{Int8}(10000, 10000)
#   D = Array{Int8}(10000, 10000)
#   for i = 1:10000
#     for j = 1:10000
#       A[i, j] = Int8(0)
#     end
#   end
#   for i = 1:10000
#     for j = 1:10000
#       B[i, j] = A[i, j] + Int8(1)
#     end
#   end
#   for i = 1:10000
#     for j = 1:10000
#       C[i, j] = B[i, j] + Int8(2)
#     end
#   end
#   for i = 1:10000
#     for j = 1:10000
#       D[i, j] = C[i, j] + Int8(3)
#     end
#   end
#   return D
# end

function loopIt2()
  D = Array{Int8}(10000, 10000)
  tic()
  A = Array{Int8}(10000, 10000)
  B = Array{Int8}(10000, 10000)
  C = Array{Int8}(10000, 10000)
  for i = 1:10000
    for j = 1:10000
      A[i, j] = Int8(0)
    end
  end
  for i = 1:10000
    for j = 1:10000
      B[i, j] = A[i, j] + Int8(1)
    end
  end
  for i = 1:10000
    for j = 1:10000
      C[i, j] = B[i, j] + Int8(2)
    end
  end
  for i = 1:10000
    for j = 1:10000
      D[i, j] = C[i, j] + Int8(3)
    end
  end
  println("Julia execution time ", toq())
  return D
end

function main()
    tic()
    R_tiramisu = loopIt()
    println("Tiramisu ", toq())


    tic()
    R_regular = loopIt2()
    println("Julia ", toq())

    for i = 1:10000
      for j = 1:10000
        if R_tiramisu[i, j] != 6
          println((i, j), " ", R_tiramisu[i, j])
          break
        end
      end
    end

    println(R_tiramisu == R_regular)
    # println(D)

end

main()
