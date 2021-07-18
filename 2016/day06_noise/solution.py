import sys
from operator import itemgetter

def count_freq(string):
    freqs = {}
    for char in string:
        if char in freqs: freqs[char] += 1
        else: freqs[char] = 1
    return freqs

def cancel_noise(data, frequent):
    noise_cancelled = ''
    for chars in zip(*data):
        most_frequent = sorted(count_freq(chars).items(),
                               key=itemgetter(1),
                               reverse=frequent)[0][0]
        noise_cancelled += most_frequent
    return noise_cancelled

if __name__ == '__main__':
    data = sys.stdin.read().split('\n')[:-1]
    print(cancel_noise(data, True))
    print(cancel_noise(data, False))
