#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>
#include <cmath>
#include <variant>
#include "./superscript.h"

using namespace std;

class monomial{
	private:
		double exponent;
		double coefficient;
	public:
		//constructors
		monomial(double init_coefficient = 1.0, double init_exponent = 1.0) 
			: exponent(init_exponent), coefficient(init_coefficient){};
		
		//
		operator double() const {
        		return coefficient;  // Or any other logic to convert your monomial to a double
    		}

		//getters
		double get_exponent() const {return exponent;}
		double get_coefficient() const {return coefficient;}

		double evaluate(double x){
			return coefficient * pow(x, exponent);
		}

		string evaluate_explicit(double x){
			return  to_string(coefficient) + "(" + to_string(x) +  ")" + to_superscript_double(exponent);
		}
};

class polynomial{
	private:
		vector<monomial> f;
	public:
		polynomial() {}
};

ostream& operator << (ostream& output_stream, const monomial& some_monomial){
	if(some_monomial.get_coefficient() == 0){
		output_stream << 0;
	} else {
		if (some_monomial.get_coefficient() != 1){
			output_stream << some_monomial.get_coefficient();
		}
		if (some_monomial.get_exponent() == 0){
			//pass
		} else if(some_monomial.get_exponent() == 1) {
			output_stream << 'x';
		} else {
			output_stream << 'x';
			output_stream << to_superscript_double(some_monomial.get_exponent());
		} 
	}
	return output_stream;
} 

ostream& operator << (ostream& output_stream, const polynomial& some_polynomial){
	for(monomial : some_polynomial){

	}
}

monomial operator + (const monomial& augend, const monomial& addend)  {
	if (augend.get_exponent() == addend.get_exponent()){
		return monomial(augend.get_coefficient() + addend.get_coefficient(), addend.get_exponent());
	} else {
		throw invalid_argument("bad");	
	} 
}


int main(){
	monomial a(3.0,0.0);
	monomial b(5.0,0.0);
	monomial c(3.0,0.0);
	cout << a + b  << endl;
	return 0;
}
