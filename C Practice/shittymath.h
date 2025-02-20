// shittymath.h


#ifndef SHITTYMATH_H
#define SHITTYMATH_H

//its just pi
const double pi = 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664159265358979323846;

//the sign function
int sig(double poop){
        if (poop >= 0) {return 1;}
        else return -1;
        }

//the ceil function
int cei(double x) {
        int pooinloo = (int) x;
        if (x <= 0 || (double) pooinloo == x) {return pooinloo;}
        else {return pooinloo + 1;}
        }

//the floor function
int flo(double x) {
        int allah = (int) x;
        if (x >= 0 || (double) allah == x) {return allah;}
        else {return allah - 1;}
        }

//the modulo operator
double modu(double ass, double tits) {
        if (tits == 0.0) {return ass;}

        double feet = ass / tits;

        if (feet < 0) {feet = cei(feet);}
        else {feet = flo(feet);}
        return ass - feet * tits;
        }

//the absolute value function
double absolute(double cum) {
        if (cum<0) {return -cum;}
        else {return cum;}
        }

//the square root function
double squirt(double arg){
        double guess = arg / 2.0;
        double dif = 1.0;
        while(dif > 1e-16){
                double nguess = 0.5 * (guess + arg / guess);
                dif = absolute(nguess - guess);
                guess = nguess;
        }
        return guess;
}

//the factorial function
double factorial(int x) {
        if(x == 0) {return 1;}
        else {
        int result = 1;
        for(int i = 1; i <= x; i++) {result *= i;} return result;}
        }

//exponential of the form x^y
double expo(double x, int y) {
        double result = 1.0;
        if (y ==0) {return 1.0;}
        else if (y>0) {for (int i = 1; i <= y; i++){result *= x;}}
        else if (y<0) {for(int i = 1; i <= -y; i++) {result /= x;}}
        return result;
        }

//the sin function
double sine(double angle){
        double pi = 3.14159265358979323846;
        angle = modu(angle, 2.0 * pi);

        if (angle < -pi + 1e-12) {angle += 2.0 * pi;}
        else if (angle > pi - 1e-12) {angle -= 2.0 * pi;}

        double term = angle;
        double sum = 0;
        int i = 0;

        while(absolute(term) > 1e-16){
        sum += term;
        term *= (-1.0) * angle * angle / ((2 * i + 2) * (2 * i + 3));
        i++;
        }
        return sum;
        }

//the cosine function
double cosi(double angle){
        double pi = 3.14159265358979323846;
        angle = modu(angle, 2.0 * pi);

        if (angle < -pi + 1e-12) {angle += 2.0 * pi;}
        else if (angle > pi - 1e-12) {angle -= 2.0 * pi;}

        double term = 1;
        double sum = 0;
        int i = 0;

        while(absolute(term) > 1e-16){
        sum += term;
        term *= (-1.0) * angle * angle / ((2 * i + 2) * (2 * i + 1));
        i++;
        }
        return sum;
        }

//the tangent function
double tang(double angle) {return sine(angle) / cosi(angle);}

//the cotangent function
double cota(double angle) {return cosi(angle) / sine(angle);}

//the secant function
double seca(double angle) {return 1 / cosi(angle);}

//the cosecant function
double cose(double angle) {return 1 / sine(angle);}

//the hyperbolic sine function
double sinh(double hangle){
        double term = hangle;
        double sum = 0;
        int i = 0;

        while(term > 1e-16) {
        sum += term;
        term *= hangle * hangle / ((2 * i + 2) * (2 * i + 3));
        i++;
        }
        return sum;
        }

//the hyperbolic cosine function
double cosh(double hangle){
        double term = 1;
        double sum = 0;
        int i = 0;

        while(term > 1e-16) {
        sum += term;
        term *= hangle * hangle / ((2 * i + 2) * (2 * i + 1));
        i++;
        }
        return sum;
        }

//the hyperbolic tangent function
double tanh(double hangle){
        if (absolute(hangle) > 710) {return sig(hangle);}
        else if(hangle >= 0) {return sinh(hangle) / cosh (hangle);}
        else return -(sinh(-hangle) / cosh(-hangle));
        }

//the hyperbolic cotangent function
double coth(double hangle){return 1/tanh(hangle);}

//the hyperbolic secant function
double sech(double hangle){return 1/cosh(hangle);}

//the hyperbolic cosecant function
double csch(double hangle){
        if(hangle >= 0){return 1/sinh(hangle);}
        else return -(1/sinh(-hangle));
        }

//exponential
double e(double exp) {return (sinh(exp) + cosh(exp));}

//the exponential function
double etothe(double expnt){
        double term = 1;
        double sum = 0;
        int i = 0;

        while (term > 1e-16) {
        sum += term;
        i++;
        term *= expnt / i;
        }
        return sum;
        }

//the natural logarithm function
double lan(double x) {
        double y = x - 1.0;
        int iter = 0;

        while (absolute(e(y) - x) > 1e-16 && iter < 1000) {
                double f = e(y) - x;
                y = y - 2.0 * f * e(y) / (2.0 * e(y) * e(y) - f * e(y));
                iter++;
        }
        return y;
}


#endif // SHITTYMATH_H
