#program to calculate the square root
import math
while True:	
	N = float(input('Enter the number you want the square root for: '))
	P = float(input('Enter the precision to which you want the square root : '))
	if N<0 or (P<0 or P>1):
		print 'Please input a valid input number or precision'
		continue
	else:
		break
print 'number - ',N
print'required precision - ',P

def testfun():
	print 'function tested'
	return

testfun()

def AM(x,y):
	arithmetic_mean_with_1 = (x+y)/2
	return(arithmetic_mean_with_1)

# print AM(N,1)

precision_digits = math.log(P,0.1)

def truncate(x,d):
	return int(x*(10.0**d))/(10.0**d)
# print truncate(1.123456789,precision_digits)	

def calculation(number,p):
	upper_bound = AM(number,1)
	lower_bound = 1
	number_of_iterations = 1
	while True:
		if (upper_bound-lower_bound) > p:
			print 'upper bound in step',number_of_iterations,'is',upper_bound
			# print 'lower bound is :',lower_bound
			lower_bound = number/upper_bound
			upper_bound = AM(upper_bound,lower_bound)
			number_of_iterations = number_of_iterations + 1
			continue
		else:	
			break
	print 'final upper bound :',upper_bound
	print 'Number of iterations :',number_of_iterations
	print 'square root of the number is :',truncate(upper_bound,precision_digits)
calculation(N,P)