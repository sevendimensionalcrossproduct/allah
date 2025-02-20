#include <iostream>
#include <cmath>
#include <vector>

class quaternion{
	private: 
		double real;
		double i, j, k;

	public: 
		//quaternion constructor
		quaternion(double init_real = 0.0, double init_i = 0.0, double init_j = 0.0, double init_k = 0.0) : real(init_real), i(init_i), j(init_j), k(init_k) {}

		//quaternion extractor
		double get_i() const {return i;}
		double get_j() const {return j;}
		double get_k() const {return k;}

		double get_real() const {return real;}
		std::vector<double> get_vector() const {return {i, j, k};}

		//arithmetic operator overloading
		quaternion operator + (const quaternion& argument) const {
			return quaternion (real + argument.real, i + argument.i, j + argument.j, k + argument.k);
		}

		quaternion operator - (const quaternion& argument) const{
			return quaternion (real - argument.real, i - argument.i, j - argument.j, k + argument.k);
		}

		quaternion operator * (const quaternion& argument) const{
			return quaternion ( 
				real * argument.real - i * argument.i - j * argument.j - k * argument.k,
				real * argument.i + i * argument.real + j * argument.k - k * argument.j,
				real * argument.j - i * argument.k + j * argument.real + k * argument.i, 
				real * argument.k + i * argument.j - j * argument.i + k * argument.real
				);
		}
};

//output stream overloading
std::ostream& operator << (std::ostream& outputStream, const quaternion& someQuaternion){
	outputStream << someQuaternion.get_real();
    
    	outputStream << (someQuaternion.get_i() >= 0.0 ? " + " : " - ") << std::abs(someQuaternion.get_i()) << "i";
    	outputStream << (someQuaternion.get_j() >= 0.0 ? " + " : " - ") << std::abs(someQuaternion.get_j()) << "j";
    	outputStream << (someQuaternion.get_k() >= 0.0 ? " + " : " - ") << std::abs(someQuaternion.get_k()) << "k";

	return outputStream;
}

//scalar arithmetic overloading
quaternion operator * (double scalar, const quaternion &someQuaternion){
	return quaternion(scalar * someQuaternion.get_real(), scalar * someQuaternion.get_i(), scalar * someQuaternion.get_j(), scalar * someQuaternion.get_k());
}

quaternion operator / (const quaternion &someQuaternion, double scalar){
	return quaternion(someQuaternion.get_real()/scalar, someQuaternion.get_i()/scalar, someQuaternion.get_j()/scalar, someQuaternion.get_k()/scalar);
}

quaternion conjugate(const quaternion& someQuaternion){
	return quaternion(someQuaternion.get_real(), -someQuaternion.get_i(), -someQuaternion.get_j(), -someQuaternion.get_k());
}

double norm(const quaternion& someQuaternion){ 
	return std::sqrt((someQuaternion * conjugate(someQuaternion)).get_real());
}

quaternion versor(const  quaternion& someQuaternion){
	double someNorm = norm(someQuaternion);
	return quaternion(someQuaternion.get_real()/someNorm,someQuaternion.get_i()/someNorm,someQuaternion.get_j()/someNorm,someQuaternion.get_k()/someNorm);
}

quaternion reciprocal(const quaternion& someQuaternion){
	double normSquared = norm(someQuaternion) * norm(someQuaternion);
	return conjugate(someQuaternion) / normSquared;
}

int main(){
	quaternion i(0.0,1.0,0.0,0.0);
	quaternion j(0.0,0.0,1.0,0.0);
	quaternion k(0.0,0.0,0.0,1.0);

	quaternion q(1.0,1.0,1.0,1.0);
	
	std::cout << "The result is: " << reciprocal(k) << std::endl;

	return 0;
}
