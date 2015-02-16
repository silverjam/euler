#!/usr/bin/env python2

words = "please reverse me for great win"
words2 = "please reverse me for great wins"


def indexes(start, stop):

    diff = (stop - start) + 1
    swap_low = range(start, start + diff/2)
    swap_high = range(stop, stop - diff/2, -1)

    return zip(swap_low, swap_high)


def revstr(arr, start = None, stop = None):

    for swap_low, swap_high in indexes(start, stop):
        temp = arr[swap_low]
        arr[swap_low] = arr[swap_high]
        arr[swap_high] = temp

    return arr


def revwords(w):

    arr = list(w)
    max_index = len(arr) - 1
    revstr(arr, start = 0, stop = max_index)

    last_index, idx = 0, 0
    while idx <= max_index:
        if arr[idx] == ' ':
            revstr(arr, start = last_index, stop = idx - 1)
            last_index = idx + 1
        idx += 1

    revstr(arr, stop = idx - 1, start = last_index)

    return arr


print str.join('', revwords(words))
print str.join('', revwords(words2))
