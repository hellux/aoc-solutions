from hashlib import md5

def match_iterate_hex(seq_start, hex_start, index):
    md5_sum = ''
    while md5_sum[:len(hex_start)] != hex_start:
        string = (seq_start + str(index)).encode('utf-8')
        md5_sum = md5(string).hexdigest()
        index += 1

    return md5_sum, index

def find_password(password_length, hex_start, seq_start, pw_assign):
    password, index = '_'*password_length, 0
    while password.find('_') != -1:
        while True:
            md5_sum, index = match_iterate_hex(
                seq_start,
                hex_start,
                index,
            )
            new_password = pw_assign(password, md5_sum)
            if password != new_password:
                password = new_password
                break
    return password

def part1(door_id):
    def pw_assign(pw, md5):
        index = pw.find('_')
        char = md5[5]
        return pw[:index] + char + pw[index+1:]

    return find_password(8, '00000', door_id, pw_assign)

def part2(door_id):
    def pw_assign(pw, md5):
        index = md5[5]
        if index.isnumeric() and \
           int(index) < len(pw) and \
           pw[int(index)] == '_':
            index = int(index)
            char = md5[6]
            return pw[:index] + char + pw[index+1:]
        else:
            return pw

    return find_password(8, '00000', door_id, pw_assign)

if __name__ == '__main__':
    door_id = input()
    print(part1(door_id))
    print(part2(door_id))
