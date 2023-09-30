J= 1721130
#J= 2460165

#J= 1720060

for J in range(1721130, 2460165, 10):
	print(J)
	y = 4716
	v = 3
	j = 1401
	u =	5
	m = 2
	s =	153
	n = 12
	w =	2
	r = 4
	B =	274277
	p = 1461
	C =	-38
	f = J + j + (((4 * J + B) // 146097) * 3) // 4 + C
	e = r * f + v
	g = (e % p) // r
	h = u * g + w
	D = ((h % s)) // u + 1
	M = ((h // s + m) % n) + 1
	Y = (e // p) - y + (n + m - M) // n
	print("The Gregorian date(D, M, Y) is: ", D, M, Y)
	if D < 1 or D > 31:
		raise Exception("Invalid day")
	if M < 1 or M > 12:
		raise Exception("Invalid month")
	

