#include <iostream>
#include <cstdio>
#include <sstream>
#include <string>
#include <vector>
#include <stack>
#include <cctype>
#include <stdexcept>
#include <cmath>

enum token_type { NUMBER, OPERATOR };
enum associativity { LEFT, RIGHT };

struct token {
    token_type type;
    double value;
    char opperateur;

    token(double number_value) : type(NUMBER), value(number_value), opperateur(0) {}
    token(char operator_type) : type(OPERATOR), value(0), opperateur(operator_type) {}
};

int get_precedence(char some_operator){
    switch(some_operator){
        case '+': 
        case '-':  
            return 1;
        case '*': 
        case '/': 
            return 2;
        case '^':
            return 3;
        default: return 0;
    }
}

associativity get_associativity(char some_operator){
    switch(some_operator){
        case '+':case '-': case '*': case '/':
            return LEFT;
        case '^':
            return RIGHT;
        default:
            return LEFT;
    }
}

void print_token(const token &some_token){
    if(some_token.type == NUMBER){
        std::cout << "Num:" << some_token.value;
    } else if (some_token.type == OPERATOR){
        std::cout << "Op:" << some_token.opperateur << " p=" << get_precedence(some_token.opperateur);
    }
}

void print_tokens(const std::vector<token> &tokens){
    printf("<");
    for (size_t i = 0; i < tokens.size(); i++){
        print_token(tokens[i]);
        if (i < tokens.size() -1){
            printf(", ");
        }
    }
    printf(">");
}

std::string show_expression(const std::vector<token> &tokens) {
    std::ostringstream expression;
    for (const token &current_token : tokens) {
        if (current_token.type == NUMBER) {
            expression << current_token.value << " ";
        } else if (current_token.type == OPERATOR) {
            expression << current_token.opperateur << " ";
        }
    }
    return expression.str();
}

std::vector<token> tokenize(const std::string &expression){
    std::vector<token> tokens;
    std::istringstream iss(expression);
    char c;

    while (iss >> c) {
        if(std::isdigit(c)){
            iss.putback(c);
            double number;
            iss >> number;
            tokens.emplace_back(token(number));
        } else if (c == '+' || c == '-' || c == '*' || c == '/' || c == '(' || c == ')' || c == '^'){
            tokens.emplace_back(c);
        } else {
            throw std::runtime_error(std::string("Unknown Token: ") + c);
            return {};
        }
    }

    return tokens;
}

std::vector<token> to_reverse_polish(const std::vector<token> &tokens){
    std::vector<token> output;
    std::stack<token> operators;

    for (const token &current_token : tokens){
        if (current_token.type == NUMBER) {
            output.emplace_back(current_token);
        } else if (current_token.type == OPERATOR){
            if (current_token.opperateur == '(') {
                operators.push(current_token);
            } else if (current_token.opperateur == ')') {
                while (!operators.empty() && operators.top().opperateur != '(') {
                    output.emplace_back(operators.top());
                    operators.pop();
                }
            if (operators.empty()){ throw std::runtime_error("Syntax Error: Mismatched Closing Parenthesis");}
            operators.pop(); //Pop closing parenthesis
            } else {
                while(!operators.empty() && 
                    (get_precedence(operators.top().opperateur) > get_precedence(current_token.opperateur) || 
                    (get_precedence(operators.top().opperateur) == get_precedence(current_token.opperateur) &&
                    get_associativity(current_token.opperateur) == LEFT))) {
                        output.emplace_back(operators.top());
                        operators.pop();
                }
                operators.push(current_token);
            }
        }
    }
    
    while(!operators.empty()){
        if(operators.top().opperateur == '(') { throw std::runtime_error("Syntax Error: Mismatched Opening Parenthesis");}
        output.emplace_back(operators.top());
        operators.pop();
    }
    return output;
}

double evaluate_reverse_polish(const std::vector<token>& tokens){
    std::stack<double> stack;

    for(const token& current_token : tokens){
        if(current_token.type == NUMBER) {
            stack.push(current_token.value);
        } else if (current_token.type == OPERATOR){
            if (stack.size() < 2) throw std::runtime_error("Syntax Error: Invalid Expression");
            double rhs = stack.top(); stack.pop();
            double lhs = stack.top(); stack.pop();

            double result;
            switch (current_token.opperateur) {
                case '+': result = lhs + rhs; break;
                case '-': result = lhs - rhs; break;
                case '*': result = lhs * rhs; break;
                case '/': 
                    if(rhs == 0) throw std::runtime_error("Math Error: Division by zero");
                    result = lhs / rhs; break;
                case '^': result = std::pow(lhs,rhs); break;
                default: throw std::runtime_error("Unknown operator");
            }
            stack.push(result);
        }
    }
    if (stack.size() != 1) throw std::runtime_error("Syntax Error: Invalid Expression");
    return stack.top();
}

int main(){
    std::string input;
    std::cout << ("Expression: ");
    std::getline(std::cin, input);
    
    std::vector<token> tokens = tokenize(input);
    std::string output_expression = show_expression(to_reverse_polish(tokens));

    printf("Tokenized Input: ");
    print_tokens(tokens);

    printf("\nReverse Polish Notation: ");
    std::cout << output_expression;

    printf("\nTokenized Output: ");
    print_tokens(tokenize(output_expression));

    printf("\nResult: ");
    std::cout << evaluate_reverse_polish(tokenize(output_expression)) << "\n";
}
