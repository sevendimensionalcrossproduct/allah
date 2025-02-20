#include<iostream>
#include <cmath>
#include <vector>
#include <functional>

class dualNumber {
	private: 
	//only members of dualNumber can have this
	double real;
	double epsilon;

	public: 
	using dualFunction =  std::function<dualNumber(std::vector<dualNumber>&)>;
	using dualVector = std::vector<dualNumber>&;

	//dualnum constructor
	dualNumber(double initReal=0.0, double initEpsilon=0.0) : real(initReal), epsilon(initEpsilon){} 

	//extractor member funcitons
	double getReal() const {return real;}
	double getEpsilon() const {return epsilon;}


	//overloading bs etc
	dualNumber operator+(const dualNumber& arg) const {
		return dualNumber(real + arg.real, epsilon + arg.epsilon);
	}

        dualNumber operator-(const dualNumber& arg) const {
                return dualNumber(real - arg.real, epsilon - arg.epsilon);
        }

        dualNumber operator*(const dualNumber& arg) const {
                return dualNumber(real * arg.real, (real * arg.epsilon) + (epsilon * arg.real));
        }

        dualNumber operator/(const dualNumber& arg) const {
                return dualNumber(real / arg.real, ((epsilon * arg.real) - (real * arg.epsilon))/(arg.real * arg.real));
	}

	//declare functions so that the compiler doesn't bitch about scope fuck tardedness
	dualNumber dusin(const dualNumber& dual);
	dualNumber ducos(const dualNumber& dual);
	dualNumber dutan(const dualNumber& dual);
	dualNumber ducot(const dualNumber& dual);
	dualNumber dusec(const dualNumber& dual);
	dualNumber ducsc(const dualNumber& dual);

	dualNumber duarcsin(const dualNumber& dual);
	dualNumber duarccos(const dualNumber& dual);
	dualNumber duarctan(const dualNumber& dual);

	dualNumber duexp(const dualNumber& dual);
	dualNumber dulog(const dualNumber& dual);
	
	dualNumber squirt(const dualNumber& dual);
	dualNumber recip(const dualNumber& dual);

	dualNumber expofuck(const dualNumber& base, const dualNumber& exponent);

	dualNumber gradientDescent(dualVector startingPoint, dualFunction someFunction, int iter, double stepSize);
};

//define trigonometry for dual nums
dualNumber dusin(const dualNumber& dual){
	double resultReal = std::sin(dual.getReal());
	double resultEpsilon = std::cos(dual.getReal()) * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

dualNumber ducos(const dualNumber& dual){
	double resultReal = std::cos(dual.getReal());
	double resultEpsilon = std::sin(dual.getReal()) * -1.0 * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

dualNumber dutan(const dualNumber& dual){
	double resultReal = std::tan(dual.getReal());
	double resultEpsilon = 1.0 / (std::cos(dual.getReal()) * std::cos(dual.getReal())) * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

dualNumber ducot(const dualNumber& dual){
	double resultReal = 1.0 / std::tan(dual.getReal());
	double resultEpsilon = -1.0 / (std::sin(dual.getReal()) * std::sin(dual.getReal())) * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

dualNumber dusec(const dualNumber& dual){
	double resultReal = 1.0 / std::cos(dual.getReal());
	double resultEpsilon = std::tan(dual.getReal()) / std::cos(dual.getReal()) * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

dualNumber ducsc(const dualNumber& dual){
	double resultReal = 1.0 / std::sin(dual.getReal());
	double resultEpsilon = -1.0 / (std::sin(dual.getReal()) * std::tan(dual.getReal())) * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

//define inverse trigonometry for dual nums
dualNumber duarcsin(const dualNumber& dual){
	double resultReal = std::asin(dual.getReal());
	double resultEpsilon = 1.0 / (std::sqrt(1.0 - dual.getReal() * dual.getReal())) * dual.getEpsilon();
	return dualNumber(resultReal, resultEpsilon);
}

dualNumber duarccos(const dualNumber& dual){
	double resultReal = std::acos(dual.getReal());
	double resultEpsilon = -1.0 / (std::sqrt(1.0 - dual.getReal() * dual.getReal())) * dual.getEpsilon();
	return dualNumber(resultReal, resultEpsilon);
}

dualNumber duarctan(const dualNumber& dual){
	double resultReal = std::atan(dual.getReal());
	double resultEpsilon = 1.0 / (1.0 + dual.getReal() * dual.getReal());
	return dualNumber(resultReal, resultEpsilon);
}

//misc functions
dualNumber squirt(const dualNumber& dual){
	double resultReal = std::sqrt(dual.getReal());
	double resultEpsilon = 1.0 / (2.0 * std::sqrt(dual.getReal())) * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

dualNumber duexp(const dualNumber& dual){
	double resultReal = std::exp(dual.getReal());
	double resultEpsilon = std::exp(dual.getReal()) * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

dualNumber dulog(const dualNumber& dual){
	double resultReal = std::log(dual.getReal());
	double resultEpsilon = (1.0 / dual.getReal()) * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

dualNumber recip(const dualNumber& dual){  //reciprocal of a function
	double resultReal = 1.0 / dual.getReal();
	double resultEpsilon = -1.0 / (dual.getReal() * dual.getReal()) * dual.getEpsilon();
	return dualNumber(resultReal,resultEpsilon);
}

dualNumber expofuck(const dualNumber& base, const dualNumber& exponent){ //functional exponential
	double resultReal = exp(exponent.getReal() * log(base.getReal()));
	double resultEpsilon = exp(exponent.getReal() * log(base.getReal())) * ((exponent.getEpsilon() * log(base.getReal()) + (exponent.getReal() * base.getEpsilon() / base.getReal())));
	return dualNumber(resultReal,resultEpsilon);
}

// transforms double type vectors into double type vectors
std::vector<dualNumber> vectorToDualVector(const std::vector<double>& inputVector){
	std::vector<dualNumber> outputVector;
	outputVector.reserve(inputVector.size());
	
	for (const double& entryValue : inputVector){
	outputVector.emplace_back(entryValue, 0.0);
	} 
	return outputVector;
}

//gradient operator
std::vector<double> gradient(std::vector<double>& somePoint, dualNumber::dualFunction someFunction){
	std::vector<double> partialVector(somePoint.size(), 0.0);
	std::vector<dualNumber> somePointDual = vectorToDualVector(somePoint);

	for(int a = 0; a < somePoint.size();a++){
		somePointDual[a] = somePointDual[a] + dualNumber(0.0,1.0);
		dualNumber derivative = someFunction(somePointDual);
		somePointDual[a] = somePointDual[a] - dualNumber(0.0,1.0);

		partialVector[a] = derivative.getEpsilon();
	}
	return partialVector;
}

//gradient descent algorithm
std::vector<double> gradientDescent(std::vector<double>& startingPoint, dualNumber::dualFunction someFunction, double stepSize, int iterSize){
	std::vector<double> nextPoint(startingPoint.size(), 0.0);
	
	for(int e = 0; e <= iterSize; e++){
		std::vector<double> gradientAtPoint = gradient(startingPoint, someFunction);
		
		for(int q = 0; q < startingPoint.size(); q++) {
			nextPoint[q] = startingPoint[q] - stepSize * gradientAtPoint[q];
			startingPoint[q] = nextPoint[q];
		}
	}
	return nextPoint;

}

std::ostream& operator<<(std::ostream& os, const dualNumber& dualnum) {
	os << dualnum.getReal();
	os << (dualnum.getEpsilon() >= 0.0 ? " + " : " - ") << std::abs(dualnum.getEpsilon()) << "ε";
	
	return os;
}



// some function
dualNumber whatever(dualNumber::dualVector variable){
	dualNumber function = dusin(variable[0]) + ducos(variable[1]);
	return function;
}

int main(void) {
	dualNumber x(3.0,1.0); // imaginary part set to 1.0 or else it stretches the derivative
	//dualNumber y(3.0,1.0);	
	dualNumber test(1.0,-1.0);

	std::vector<double> startingPoint = {
	0.0,
	0.0,
	};

	double result = gradientDescent(startingPoint, whatever, 0.5, 100)[0];

	std::cout << "c:" << result << std::endl;
	//std::cout << test << std::endl;
} 
