all: timehack memhack
timehack: timehack.o sysapp.o
memhack: memhack.o sysapp.o


all: memhack timehack

run: timehack 
	./timehack

clean:
	rm -f *.o *~ memhack timehack
