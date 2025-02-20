#include <iostream>
#include <string>
#include <fstream>

using namespace std;

string generateBoilerplate(){
	string boilerplate = 
R"(<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
    	<title>Document</title>
</head>
<body>
    
</body>
</html>)";

	return boilerplate;
}

bool fileExists(const string& filePath){
	ifstream checkFile(filePath);
	return checkFile.good();
}

bool confirmOverwrite(const string& filePath){
	cout << "Overwrite? (y/n, default n): ";
	
	string confirmation;
	getline(cin, confirmation);

	return confirmation.empty() ? false : (confirmation == "y" || confirmation == "Y");
}

void saveFile(string& code){
	string filePath("./index.html");
	
	if (fileExists(filePath)){
		if (!confirmOverwrite(filePath)){
			cout << "Aborted." << endl;
			return;
		}
	}
	
	ofstream fileOutput(filePath);
	
	if (fileOutput.is_open()) {
		fileOutput << code;
		fileOutput.close();
		
		cout << "Generated index.html successfully." << endl;
	} else {
		cerr << "Error: Unable to generate index.html." << endl;
	}
}

int main(){
	string code = generateBoilerplate();
	saveFile(code);

	return 0;
}
