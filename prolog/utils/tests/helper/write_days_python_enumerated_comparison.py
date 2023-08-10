
from datetime import datetime


def write_asserts():
	with open(f"days_python_enumerated_comparison.pl", "w") as f:
		dmin = datetime.fromisoformat('1900-01-01').date().toordinal()
		dmax = datetime.fromisoformat('2123-01-01').date().toordinal()
		for day in range(dmin, dmax):
			python_date = datetime.fromordinal(day).date()
			f.write(f""":-d({day},date({python_date.year},{python_date.month},{python_date.day})).\n""")

write_asserts()
