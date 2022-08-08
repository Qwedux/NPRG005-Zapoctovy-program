def format(s):
    tabs = 0
    lastchar = ''
    for i in s:
        if i == '[':
            tabs += 1
            print(i+"\n", end = '')
        elif i == ']':
            tabs -= 1
            if lastchar != '[':
                print("\n", end = '')
            for _ in range(tabs):
                print('    ', end = '')
            print(i, end = '')
        elif i == ',':
            print(i+"\n", end = '')
            for _ in range(tabs):
                print('    ', end = '')
        else:
            if lastchar == '[':
                for _ in range(tabs):
                    print('    ', end = '')
            print(i, end='')
        lastchar = i

inp = input()
print()
format(inp)