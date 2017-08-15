using ParallelAccelerator

@acc function loopIt()
  # t = 0
  # for i = 1:10
  #   t = 1
  # end
  # for i = 1:5
  #   for j = 3:15
  #     t = 5
  #   end
  # end
  # return t
  A = Array{Int64}(15, 14, 17)
  B = Array{Int64}(15, 14, 17)
  for i = 1:15
    for j = 1:14
      for k = 1:17
        A[i, j, k] = 0
      end
    end
  end
  for i = 1:15
    for j = 1:14
      for k = 1:17
        B[i, j, k] = A[i, j, k] + 1
      end
    end
  end
  return B
end

function main()
    tic()
    D = loopIt()
    println("SELFTIMED ", toq())

    println(D)

end

main()
