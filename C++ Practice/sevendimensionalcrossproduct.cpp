#include <cstdlib>
#include <iostream>
#include <vector>

using namespace std;

#define START(n) ((i + n) % 7)

std::vector<double> seven_dimensional_cross_product(const std::vector<double>& x, const std::vector<double>& y){ //calculates x × y
	if (x.size() != 7 || y.size() != 7){
		cerr << ("Input vectors must be seven dimensional.") << endl;
		exit(1);
	}

	std::vector<double> result(7,0.0);
	
	for(int i = -1; i < 6; i++){  
	result[START(1)] = (x[START(2)]*y[START(4)] - x[START(4)]*y[START(2)] + x[START(3)]*y[START(7)] - x[START(7)]*y[START(3)] + x[START(5)]*y[START(6)] - x[START(6)]*y[START(5)]); //lmao
	}

	return result;
}

double seven_dimensional_dot_product(const vector<double>& x, const vector<double>& y){ //calculates x · y
	if (x.size() != 7 || y.size() != 7){
		throw std::invalid_argument("Input vectors must be seven dimensional.");
		exit(1);
	}

	double result;

	for(int i = 0; i < 7; i++){
		result += x[i] * y[i];  
	}

	return result;
}

int main(void){
	std::vector<double> x{1.0,1.0,0.0,0.0,0.0,1.0,1.0};
	std::vector<double> y{0.0,1.0,0.0,0.0,1.0,0.0,0.0};
	
	std::vector<double> result = seven_dimensional_cross_product(x,y);
	double xResult = seven_dimensional_dot_product(result,result);


	cout << xResult << endl;
}
