
from datetime import datetime


def write_asserts():
	with open(f"days_python_enumerated_comparison.pl", "w") as f:

		def rata_die_date(day_rata_die):
			return datetime.fromordinal(day_rata_die).date()
		def w(j,y,m,d):
			f.write(f"""d({j},{y},{m},{d}). """)

		day1 = rata_die_date(1)
		w(1, day1.year, day1.month, day1.day)
		f.write(f"""\n""")

		# Return the date corresponding to the proleptic Gregorian ordinal, where January 1 of year 1 has ordinal 1.
		dmin = datetime.fromisoformat('1985-01-01').date().toordinal()
		if dmin == 724642:
			pass
		else:
			raise Exception(dmin)
		dmax = datetime.fromisoformat('2050-01-01').date().toordinal()

		# actually check the monotonicity of the date
		old = None

		for day_rata_die in range(dmin, dmax):

			python_date = rata_die_date(day_rata_die)

			if old is None:
				pass
			else:
				if old.day + 1 == python_date.day:
					pass
				elif old.month + 1 == python_date.month:
					f.write(f"""\n""")
				elif old.year + 1 == python_date.year:
					f.write(f"""\n""")
				else:
					raise 'hmm'

				if (python_date - old).days != 1:
					raise 'hmm'

			old = python_date

			day = day_rata_die - 724642
			w(day, python_date.year, python_date.month, python_date.day)



write_asserts()
