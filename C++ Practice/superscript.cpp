#include <iostream>
#include <cmath>
#include <string>
#include <unordered_map>

using namespace std;

unordered_map<char, string> superscript_map;

void create_map(){
    superscript_map['0'] = "⁰";
    superscript_map['1'] = "¹";
    superscript_map['2'] = "²";
    superscript_map['3'] = "³";
    superscript_map['4'] = "⁴";
    superscript_map['5'] = "⁵";
    superscript_map['6'] = "⁶";
    superscript_map['7'] = "⁷";
    superscript_map['8'] = "⁸";
    superscript_map['9'] = "⁹";
    superscript_map['.'] = "·";

}

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
	
	/*
	size_t value = result.find_last_not_of('0');
	if (value != string::npos) {
		result = result.substr(0, value + 1);
	}
	*/

	return result;
}

string hash_to_superscript(double input){
	create_map();
	string input_string = to_string(input);
	string result = "";
	
	for(char digit: input_string)	{
		result +=  superscript_map[digit];
	}
	

	return result;
}


int main(){

	//double number;
	//cin >> number;
        //cout << to_superscript_double(number) << endl; 
	
	create_map();
	cout << hash_to_superscript(8.123914152845) << endl;
	return 0;
}
