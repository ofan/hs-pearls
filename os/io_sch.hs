
fcfs (x:y:xs) i = fcfs (y:xs) $ i + abs (y+(-x))
fcfs _ i = i

