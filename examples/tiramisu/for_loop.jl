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
  t = 0
  for i=1:5
    if i == 2
      continue
    end
    for j=7:9
      t = t + i + j
      if i == 3
        t += 5
      elseif j + i == 3
        continue
      elseif i == 4
        t += 2
      else
        t += 1
      end
    end
  end
  k = 0
  for i=1:8
    for j=-14:13
      k += 2
    end
  end
  # t = 0
  # for i=1:5
  #   t = t + 1
  # end
  return t + k
end

function main()
    tic()
    D = loopIt()
    println("SELFTIMED ", toq())

    println(D)

end

main()
