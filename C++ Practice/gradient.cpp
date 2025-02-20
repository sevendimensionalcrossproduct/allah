#include <iostream>
#include "dualnumber.h"
#include <vector>

dualnumber function(dualnumber num[]){
	dualnumber function = (num[0].dusin() * num[1].duexp() * num[2].ducos() * num[3].dusin()) + (num[4] * num[5].dulog()) + (num[6].dutan()) + (num[7] * num[7] * num[8]) + (num[9]);
	//std::cout << "test: " << function << std::endl; 
	return function;
}

dualnumber* partial(dualnumber num[], int var){
	dualnumber epsilon(0.0,1.0);
	num[var] = num[var] + epsilon;
	return num;
}


int main(void){	
	dualnumber num[10]; 
		num[0] = dualnumber(1.0, 0.0);
    		num[1] = dualnumber(2.0, 0.0);
    		num[2] = dualnumber(3.0, 0.0);
    		num[3] = dualnumber(4.0, 0.0);
    		num[4] = dualnumber(5.0, 0.0);	
    		num[5] = dualnumber(6.0, 0.0);
    		num[6] = dualnumber(7.0, 0.0);
    		num[7] = dualnumber(8.0, 0.0);
    		num[8] = dualnumber(9.0, 0.0);
		num[9] = dualnumber(10.0, 0.0);


	std::cout << "cock n balls: " << function(num) << "\n" << std::endl;


	for (int q = 0; q < 10; q++){ //do this shit with the condition being q < (length of num) for the general case
		num[q] = num[q] + dualnumber(0.0,1.0);
		
		dualnumber cock = function(num);

		std::cout << "partial derivative with respect to variable " << q + 1 << ": " << cock.getepsilon() << "\n" << std::endl;
		
		num[q] = num[q] - dualnumber(0.0,1.0);
	}


	dualnumber* cock = partial(num, 3);

	for (int i = 0; i < 10; i++){
	std::cout << "num[" << i << "]: " << cock[i] << std::endl; //ignore this shit its just for testing purposes
	}
	//now do it with vectors instead of arrays bitch
}
