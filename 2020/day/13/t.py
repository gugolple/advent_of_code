pairs = [(2, 5), (3, 7)]

def mod_sol(ap, bq):
    print(ap, bq)

    p1 = pow(ap[1], -1, bq[1])
    q1 = pow(bq[1], -1, ap[1])

    aqq1 = ap[0] * bq[1] * q1
    bpp1 = bq[0] * ap[1] * p1

    pq = ap[1] * bq[1]

    y = (aqq1 + bpp1) % pq
    print(y)
    return y

mod_sol(pairs[0], pairs[1])
