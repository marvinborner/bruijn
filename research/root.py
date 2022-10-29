import math

def compress():
    num = 25390283502835092850912835091098172509218753091220958203958230958230598230598203958098098098
    numlen = len(bin(num))
    print(f"prev: {numlen}")

    com = ""
    key = ""

    i = 0
    p_total = math.inf
    while True:
        print("---")
        s_d = s_n = s_k = math.inf
        for n in range(255, 1, -1):
            for k in range(255, 1, -1):
                r = n ** k
                d = abs(num - r)
                if d < s_d:
                    s_d = d
                    s_n = n
                    s_k = k
        # key += "{:08b}{:08b}".format(s_n, s_k)
        key += "00000"
        total = len(bin(s_d)) + len(key)
        print(s_d, s_n, s_k)
        print(f"total: {total}")
        if total > p_total:
            com = s_d
            break
        p_total = total
        num = s_d
        i += 1

    print(f"iterations: {i}, prev: {numlen}, reduction by: {total / numlen}")
