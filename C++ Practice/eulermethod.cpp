#include <iostream>
#include <cmath>

//define a function y' = f(x,y)
double f(double x, double y){
	return y;
}

void eulermethod(double x0, double y0){
	//define delta x
	double h = 0.0000001;

	//implement euler's method
	std::cout << "x_0:" << x0 << std::endl;
	std::cout << "y_0:" << y0 << std::endl;
	

	for(int i = 1; i <= 10000000; i++){

	double xnext = x0 +h;
	double ynext = y0 + (h * f(x0, y0));

	x0 = xnext;
	y0 = ynext;

	std::cout << "x_" << i << ": " << xnext << "   y_" << i << ": " << ynext << std::endl;
	}
}

int main(void){
	//define a function
	

	double initialx = 0.0;
	double initialy = 1.0;

	eulermethod(initialx, initialy);
}
