import sys

str_error = ''

def one_move(x1, y1, x2, y2):
	return abs(x1 - x2) == abs(y1 - y2)

def check(str_src, str_dest, str_answ):
	global str_error
	x1, y1 = list(map(int, str_src.split()))
	x2, y2 = list(map(int, str_dest.split()))
	str_answ = str_answ.strip().lower()
	if one_move(x1, y1, x2, y2):
		if str_answ != 't':
			str_error += "{}, {}: should be 'T', but found '{}'\n".format(str_src, str_dest, str_answ)
	elif (x1 + y1) % 2 != (x2 + y2) % 2:
		if str_answ != 'nil':
			str_error += "{}, {}: should be 'NIL', but found '{}'\n".format(str_src, str_dest, str_answ)
	else:
		if not str_answ.replace(" ", "").isdigit():
			str_error += "{}, {}: should be digits, but found '{}'\n".format(str_src, str_dest, str_answ)
		i, j = list(map(int, str_answ.split()))
		if not (0 < i < 9 and \
				0 < j < 9 and \
				one_move(x1, y1, i, j) and \
				one_move(i, j, x2, y2)):
			str_error += "{}, {}: should be something else, but found '{}'\n".format(str_src, str_dest, str_answ)

try:
	input_f = open(sys.argv[1])
except:
	sys.exit(1)

for line in input_f:
	tmp = line.split('=>')
	answer = tmp[1]
	src, dest = tmp[0].split(",")
	check(src, dest, answer)

input_f.close()
if str_error:
	print(str_error)
else:
	print("answers are correct")
