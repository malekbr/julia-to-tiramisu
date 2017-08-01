using ParallelAccelerator

@acc function loopIt()
  t = 0
  for i = 1:10
    t = 1
  end
  for i = 1:5
    for j = 3:15
      t = 5
    end
  end
  return t
end

function main()
    tic()
    D = loopIt()
    println("SELFTIMED ", toq())

    println(D)

end

main()
