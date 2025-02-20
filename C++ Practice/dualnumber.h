#ifndef DUALNUMBER_H
#define DUALNUMBER_H

#include <cmath>

class dualnumber {
	private: //only members of dualnumber can have this
		double real;
		double epsilon;


	public: //dualnum constructor
		dualnumber(double init_real=0.0, double init_epsilon=0.0) : real(init_real), epsilon(init_epsilon){} 

	//extractor funcitons
	double getreal() const {return real;}
	double getepsilon() const {return epsilon;}


	//overloading bs etc
	dualnumber operator+(const dualnumber& arg) const {
		return dualnumber(real + arg.real, epsilon + arg.epsilon);
	}

        dualnumber operator-(const dualnumber& arg) const {
                return dualnumber(real - arg.real, epsilon - arg.epsilon);
        }

        dualnumber operator*(const dualnumber& arg) const {
                return dualnumber(real * arg.real, (real * arg.epsilon) + (epsilon * arg.real));
        }

        dualnumber operator/(const dualnumber& arg) const {
                return dualnumber(real / arg.real, ((epsilon * arg.real) - (real * arg.epsilon))/(arg.real * arg.real));
	}

	//declare functions so that the compiler doesn't bitch about scope fuck tardedness
	dualnumber dusin() const;
	dualnumber ducos() const;
	dualnumber dutan() const;
	dualnumber ducot() const;
	dualnumber dusec() const;
	dualnumber ducsc() const;

	dualnumber duarcsin() const;
	dualnumber duarccos() const;
	dualnumber duarctan() const;

	dualnumber duexp() const;
	dualnumber dulog() const;
	
	dualnumber squirt() const;
	dualnumber recip() const;

	dualnumber expofuck(const dualnumber base, const dualnumber exponent) const;
};

//define trigonometry for dual nums
dualnumber dualnumber::dusin() const {
	double result_real = std::sin(real);
	double result_epsilon = std::cos(real) * epsilon;
	return dualnumber(result_real,result_epsilon);
}

dualnumber dualnumber::ducos() const {
	double re = std::cos(real);
	double eps = std::sin(real) * -1.0 * epsilon;
	return dualnumber(re,eps);
}

dualnumber dualnumber::dutan() const {
	double re = std::tan(real);
	double eps = 1.0 / (std::cos(real) * std::cos(real)) * epsilon;
	return dualnumber(re,eps);
}

dualnumber dualnumber::ducot() const{
	double re = 1.0 / std::tan(real);
	double eps = -1.0 / (std::sin(real) * std::sin(real)) * epsilon;
	return dualnumber(re,eps);
}

dualnumber dualnumber::dusec() const{
	double re = 1.0 / std::cos(real);
	double eps = std::tan(real) / std::cos(real) * epsilon;
	return dualnumber(re,eps);
}

dualnumber dualnumber::ducsc() const{
	double re = 1.0 / std::sin(real);
	double eps = -1.0 / (std::sin(real) * std::tan(real)) * epsilon;
	return dualnumber(re,eps);
}

//define inverse trigonometry for dual nums
dualnumber dualnumber::duarcsin() const{
	double re = std::asin(real);
	double epsi = 1.0 / (std::sqrt(1.0 - real * real)) * epsilon;
	return dualnumber(re,epsi);
}

dualnumber dualnumber::duarccos() const{
	double re = std::acos(real);
	double epsi = -1.0 / (std::sqrt(1.0 - real * real)) * epsilon;
	return dualnumber(re,epsi);
}

dualnumber dualnumber::duarctan() const{
	double re = std::atan(real);
	double epsi = 1.0 / (1.0 + real * real);
	return dualnumber(re,epsi);
}

//misc functions
dualnumber dualnumber::squirt() const{
	double re = std::sqrt(real);
	double eps = 1.0 / (2.0 * std::sqrt(real)) * epsilon;
	return dualnumber(re,eps);
}

dualnumber dualnumber::duexp() const{
	double re = std::exp(real);
	double eps = std::exp(real) * epsilon;
	return dualnumber(re,eps);
}

dualnumber dualnumber::dulog() const{
	double re = std::log(real);
	double eps = (1.0 / real) * epsilon;
	return dualnumber(re,eps);
}

dualnumber dualnumber::recip() const{  //reciprocal of a function
	double re = 1.0 / real;
	double eps = -1.0 / (real * real) * epsilon;
	return dualnumber(re,eps);
}

dualnumber dualnumber::expofuck(const dualnumber base, const dualnumber exponent) const{ //functional exponential
	double re = exp(exponent.real * log(base.real));
	double eps = exp(exponent.real* log(base.real)) * ((exponent.epsilon * log(base.real) + (exponent.real * base.epsilon / base.real)));
	return dualnumber(re,eps);
}

std::ostream& operator<<(std::ostream& os, const dualnumber& dualnum) {
	os << dualnum.getreal() << " + " << dualnum.getepsilon() << "ε";
	return os;
}

#endif // DUALNUMBER_H
