#include <stdio.h>


typedef struct{
	double real;
	double epsilon;
}dualnumber;

//overload operators
dualnumber add(dualnumber a, dualnumber b){
        dualnumber sum;
        sum.real = a.real + b.real;
        sum.epsilon = a.epsilon + b.epsilon;
        return sum;
}

dualnumber substract(dualnumber a, dualnumber b){
        dualnumber substraction;
        substraction.real = a.real - b.real;
        substraction.epsilon = a.epsilon - b.epsilon;
        return substraction;
}

dualnumber prod(dualnumber a, dualnumber b){
        dualnumber product;
        product.real = a.real * b.real;
        product.epsilon = (a.real * b.epsilon + a.epsilon * b.real);
        return product;
}

dualnumber quot(dualnumber a, dualnumber b){
        dualnumber quotient;
        quotient.real = a.real / b.real;
        quotient.epsilon = (a.epsilon * b.real - a.real * b.epsilon) / (b.real * b.real);
        return quotient;
}

dualnumber proj(double real){
        dualnumber projection;
        projection.real = real;
        projection.epsilon = 0.0;
        return projection;
}

dualnumber power(dualnumber base, int exponent){
	dualnumber result;
	if (exponent == 0){result = proj(1.0);}
	else if (exponent < 1){result = quot(proj(1.0), power(base, -exponent));}
	else{
		result = base;
		for (int i = 1; i < exponent; i++){
		result = prod(result, base);
		}
	}
	return result;
}

dualnumber factorial(dualnumber arg){ //only works for real integer inputs
	double result=1.0;
	if (arg.real == 0){result = 1.0;}
	else{
	for(double i = 1.0; i <= arg.real; i++){
		result = result * i;
			}
		}
	return proj(result);
	}

//define sine function
dualnumber dusin(dualnumber x){
	dualnumber result;
	dualnumber term;
	
	result = proj(0.0); // Initialize result to 0

	for (long long int i = 0; i <= 50; i++){        
		//dualnumber numerator = prod(power(proj(-1.0), i), power(x, ((2 * i) + 1)));
      		//dualnumber denominator = add(prod(proj(2.0), proj(i)), proj(1.0));
        	//dualnumber term = quot(numerator, denominator);
		//term = prod(power(proj(-1.0),i),quot(proj(1),add(prod(proj(2.0), proj(i)),proj(1.0))));
		//term = power(x,(2*i)+1);
		term = prod (prod(power(proj(-1.0),i),quot(proj(1),factorial(add(prod(proj(2.0), proj(i)),proj(1.0))))),power(x,(2*i)+1));
		result = add(result,term);
	}

	return result;

}

//define exponential function
dualnumber duexp(dualnumber x){
	dualnumber result;
	dualnumber term;

	result = proj(0.0);

	for (long long int i = 0; i <= 50; i++){
		
		
		term=quot(power(x,i), factorial(proj(i)));
		result = add(result,term);
	}
	return result;
}

int main(){
        dualnumber x = {1.0,1.0}; //imaginary part always set to 1 or else it stretches the value of the derivative
	dualnumber y = {1.0,1.0};


	dualnumber result = dusin(duexp(x));
	

        printf("The result is %lf + %lfε\n", result.real, result.epsilon);
	//dualnumber arg = {5.0,0};
	//dualnumber test = factorial(arg);
	//printf("%lf!=%lf\n",arg.real,test.real);
}

