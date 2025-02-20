#include <stdio.h>
#include <math.h>

typedef struct{
	double real;
	double epsilon;
}dualnumber;

//operator overloading of dual nums
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


//define trigonometry of dual nums
dualnumber dusin(dualnumber angle){
	dualnumber result;
	result.real =sin(angle.real);
	result.epsilon = cos(angle.real) * angle.epsilon;
	return result;
}

dualnumber ducos(dualnumber angle){
        dualnumber result;
        result.real =cos(angle.real);
        result.epsilon = (-sin(angle.real)) * angle.epsilon;
        return result;
}

dualnumber dutan(dualnumber angle){
        dualnumber result;
        result.real =tan(angle.real);
        result.epsilon = (1.0/cos(angle.real)) * (1.0/cos(angle.real)) * angle.epsilon;
        return result;
}

dualnumber ducot(dualnumber angle){
        dualnumber result;
        result.real =1.0/(tan(angle.real));
        result.epsilon = -((1.0/sin(angle.real)) * (1.0/sin(angle.real))) * angle.epsilon;
        return result;
}

dualnumber dusec(dualnumber angle){
        dualnumber result;
        result.real =1.0/(cos(angle.real));
        result.epsilon = (1.0/cos(angle.real)) * tan(angle.epsilon);
        return result;
}

dualnumber ducsc(dualnumber angle){
        dualnumber result;
        result.real =1.0/(sin(angle.real));
        result.epsilon = -((1.0/sin(angle.real)) * (1.0/tan(angle.real))) * angle.epsilon;
        return result;
}

//define inverese trigonometric fucntions of dual numbers
dualnumber duarcsin(dualnumber sine){
        dualnumber arc;
        arc.real =asin(sine.real);
        arc.epsilon = (1.0/(sqrt(1.0-(sine.real)*(sine.real)))) * sine.epsilon;
        return arc;
}

dualnumber duarccos(dualnumber cosine){
        dualnumber arc;
        arc.real =acos(cosine.real);
        arc.epsilon = -(1.0/(sqrt(1.0-(cosine.real)*(cosine.real))))* cosine.epsilon;
        return arc;
}

dualnumber duarctan(dualnumber tangent){
        dualnumber arc;
        arc.real =atan(tangent.real);
        arc.epsilon = 1.0/(1.0+(tangent.real)*(tangent.real)) * tangent.epsilon;
        return arc;
}

//define square root of dual numberts

dualnumber squirt(dualnumber base){
	dualnumber result;
	result.real = sqrt(base.real);
	result.epsilon = base.epsilon / (2.0 * sqrt(base.real)); 
	return result;
}

//define exponential and natural logarithm function of dual nums
dualnumber expofun(dualnumber exponent){
	dualnumber result;
	result.real = exp(exponent.real);
	result.epsilon = exp(exponent.real)*exponent.epsilon;
	return result;
}

dualnumber natlog(dualnumber antilog){
	dualnumber result;
	result.real = log(antilog.real);
	result.epsilon = (1/antilog.real) * antilog.epsilon;
	return result;
}


//functional exponents f^g
dualnumber expofuck(dualnumber base, dualnumber exponent){
	dualnumber result;
	result.real = exp(exponent.real*(log(base.real))); //need to fix library, lan function is fucked 0<x<1, etothe x<0
	result.epsilon = exp(exponent.real * log(base.real)) * ((exponent.real * (base.epsilon/base.real)) + (exponent.epsilon * log(base.real)));
	return result;
}

//functional reciprocal rule
dualnumber recip(dualnumber pissfuck){
	dualnumber result;
	result.real = 1 / pissfuck.real;
	result.epsilon = (-pissfuck.epsilon) / ((pissfuck.real)*(pissfuck.real));
	return result;
}

int main(){
	//supports partial derivatives, create "x" and "y" etc dualnums, the imaginary part is the direction
	//of the directional deriv eg x= {1,2} y={3,4} is directional derivative in direction of vector <2,4> at (1,3)
	//for partial derivatives just let the imaginary part of the variable u want be 1 and the rest 0
	
	dualnumber x = {4.0,-1.0/sqrt(2.0)}; //imaginary part always set to 1 or else it stretches the value of the derivative
	dualnumber y = {-3.0,1.0/sqrt(2.0)};

	dualnumber function = squirt(add(prod(x,x), prod(y,y)));

	//printf("f(%lf) = %lf\n", x.real, function.real);
	//printf("f'(%lf) = %lf\n", x.real, function.epsilon);
	printf("The result is %lf + %lfε\n", function.real, function.epsilon);
}










