
(displaydist (leftDo Norm
    (prize) <- uniformDoor
    return (prize (displaydist (leftDo Norm
                                       (door) <- (host prize 'M)
                                       () <- (observe door 'L)
                                       return (door))))))
