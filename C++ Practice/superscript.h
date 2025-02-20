#ifndef SUPERSCRIPT_H
#define SUPERSCRIPT_H

#include <cmath>
#include <string>

using namespace std;


string superscript(long long int index){
	string superscripts[] = {"⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"};
	string result = "";

        if (index < 0) {
        	index = -index;
		while(index>0){
        		int digit = index % 10;
        		result = superscripts[digit] + result;
        		index /= 10;
		}
		return "⁻" + result; 
	} else if (index == 0) {
		result = "⁰";
		return result;
    	} else {
	while (index > 0) {
        	int digit = index % 10;
        	result = superscripts[digit] + result;
        	index /= 10;
    	}
	return result;
	}
}

string alternate_superscript(long long int some_number){
	string superscripts[] = {"⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"};
	string result = "";

	if(some_number < 0) {
		some_number = -some_number;
		string number_string = to_string(some_number);
		if (number_string.length() > 18){
			cerr << "error: too long" << endl;
			return "";
		}
		
		for(char digit_char : number_string) {
			int digit_int = digit_char - '0'; //turn it into an int
			result += superscripts[digit_int];
		}
		
		return "⁻" + result;
		
		} else {
		
		string number_string = to_string(some_number);
		if (number_string.length() > 18){
			cerr << "error: too long" << endl;
			return "";
		}

		for (char digit_char : number_string) {
        		int digit_int = digit_char - '0';
        		if (digit_int >= 0 && digit_int <= 9) {
            			result += superscripts[digit_int];
        		}
    		}
		return result;
	}
}

string to_superscript_double(double input){
	string superscripts[] = {"⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"};
	string result = "";

	//Characteristic to superscript
	long long int characteristic = static_cast<long long int>((floor(input)));
    	string result_characteristic = alternate_superscript(characteristic);



	//Mantissa to superscript
	
	string mantissa_str = to_string(input - floor(input)).substr(2);
	size_t value = mantissa_str.find_last_not_of('0');
	if (value != std::string::npos) {
		mantissa_str= mantissa_str.substr(0, value + 1);
	}	
	
	long long int mantissa_int = stoll(mantissa_str);

    	string result_mantissa = alternate_superscript(mantissa_int);
    	
	return result_characteristic + "·" + result_mantissa;
}

string get_mantissa(double input){
	string result = to_string(input - floor(input)).substr(2);
	
	size_t value = result.find_last_not_of('0');
	if (value != string::npos) {
		result = result.substr(0, value + 1);
	}

	return result;
}

#endif // SUPERSCRIPT_H
