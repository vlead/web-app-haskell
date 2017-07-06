
import sys, os

def nwcl(filename) :

    f = open(filename, encoding="utf-8")

    linelist = f.readlines()

    count = 0
    for i in linelist :
        print(i)
        opt1 =  (i[0:2] != "--")
        opt2 =  i.strip()
        if (opt1 and opt2) :
            count += len(i.split(" "))

    return count


if __name__ == "__main__" :

    src = "../imp/build/code"
    total = 0
    for subdir, dirs, files in os.walk(src) :
        for file in files :
            filename = os.path.join(subdir, file)
            num = nwcl(filename)
            total += num

    print("Number of words = "+ str(total))
