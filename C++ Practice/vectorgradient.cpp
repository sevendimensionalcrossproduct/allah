#include <iostream>
#include "dualnumber.h"
#include <vector>

dualnumber func(std::vector<dualnumber>& num){
	dualnumber function = (num[0].dusin() * num[1].duexp() * num[2].ducos() * num[3].dusin()) + (num[4] * num[5].dulog()) + (num[6].dutan()) + (num[7] * num[7] * num[8]) + (num[9]);
	//std::cout << "test: " << function << std::endl; 
	return function;
}

dualnumber penis(std::vector<dualnumber>& num){
		dualnumber function = num[0].dusin() + num[1].ducos();
		return function;
}

std::vector<double> gradient(std::vector<dualnumber>& num,dualnumber(*cum)(std::vector<dualnumber>&)){
	std::vector<double> partialvector(num.size() , 0.0);

	for (int q = 0; q < num.size(); q++) {
		num[q] = num[q] + dualnumber(0.0,1.0);

		dualnumber cock = cum(num);
		
		num[q] = num[q] - dualnumber(0.0,1.0);

		std::cout << "partial: " << cock.getepsilon() <<std::endl;

		partialvector[q] = cock.getepsilon();
	} 

	return partialvector;
}

int main(void){	
	std::vector<dualnumber> expl = { 
		dualnumber(1.0, 0.0),
    		dualnumber(2.0, 0.0),
    		dualnumber(3.0, 0.0),
    		dualnumber(4.0, 0.0),
    		dualnumber(5.0, 0.0),	
    		dualnumber(6.0, 0.0),
    		dualnumber(7.0, 0.0),
    		dualnumber(8.0, 0.0),
    		dualnumber(9.0, 0.0),
		dualnumber(10.0, 0.0),
	};
	
	gradient(expl,penis);

	//for (int l = 0; l < expl.size(); l++) {
	//std::cout << "asdikjkdfjksjgdk: " << gradient(expl,func)[l] << std::endl;
	//}
}
