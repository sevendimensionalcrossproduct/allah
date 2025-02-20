#include <iostream>
#include <vector>
#include "dualnumber.h"

dualnumber fxy(std::vector<dualnumber>& num){
	dualnumber function = num[0] * num[0] + (num[1].dusin() * num[1].dusin());  
	return function;
}

dualnumber fuckingfunction(std::vector<dualnumber>& num){
	dualnumber result = num[0].dusin() + num[1].ducos();
	return result;
}

std::vector<double> gradient(std::vector<dualnumber>& num , dualnumber(*cum)(std::vector<dualnumber>&)){ //this is the shit that does the magic: gradient(some point, some function)
	std::vector<double> partialvector(num.size() , 0.0);

	for (int q = 0; q < num.size(); q++) {
		num[q] = num[q] + dualnumber(0.0,1.0);

		dualnumber cock = cum(num);
		
		num[q] = num[q] - dualnumber(0.0,1.0);


		partialvector[q] = cock.getepsilon();
	} 

	return partialvector;
}

std::vector<double> descent(std::vector<dualnumber>& num , std::vector<double>& somegradient, double step){ //does a single iteration: descent(starting point, stored gradient of function, step size) 
	std::vector<double> nextpoint(num.size(),0.0);

	for (int q = 0; q < num.size(); q++){
		nextpoint[q] = num[q].getreal() - step * somegradient[q];
	}

	return nextpoint;
}



std::vector<double> thedescent(std::vector<dualnumber>& startingpoint , dualnumber(*somefunction)(std::vector<dualnumber>&) , int iter, double step){ //does many iterations of the gradient descent algorithm
	std::vector<double> nextpoint(startingpoint.size(),0.0);

	for(int q = 1; q <= iter; q++){
		std::vector<double> storedgradient = gradient(startingpoint, somefunction);
		nextpoint = descent(startingpoint, storedgradient ,step);
	
		for(int p=0; p < startingpoint.size(); p++) {
			startingpoint[p] = nextpoint[p];	
		}
	}
	

	return nextpoint;
}


int main(void){
	std::vector<dualnumber> cockle = { //the real parts of this vector is the starting point
	dualnumber(1.0,0.0),
	dualnumber(3.0,0.0),
	};
	
	std::vector<dualnumber> sonofcockle = {
	dualnumber(0.0,0.0),
	dualnumber(0.0,0.0),
	};
	
	//std::vector<double> storedgradient = gradient(sonofcockle, fuckingfunction);

	//double poopoo = descent(sonofcockle, storedgradient, 0.5)[0];

	double poopoo = thedescent(sonofcockle, fuckingfunction, 100, 0.5)[0]; //it works!! the true minimum is (-pi/2, 0), [0] is the x value and [1] is the y value

	std::cout << "fuc: " << poopoo << std::endl;
	
}
