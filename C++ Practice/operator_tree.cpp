#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <stack>
#include <cctype>
#include <stdexcept>
#include <cmath>
#include <set>
#include <unordered_map>

enum token_type { NUMBER, OPERATOR, VARIABLE };
enum associativity { LEFT, RIGHT };

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

struct token {
    token_type type;
    double value;
    char token_operator;
    char variable;

    token(double number_value) : type(NUMBER), value(number_value), token_operator(0), variable(0) {}

    token(char input) {
        if (std::isalpha(input)) {
            type = VARIABLE;
            variable = input;
            token_operator = 0;
            value = 0;
        } else {
            type = OPERATOR;
            token_operator = input;
            variable = 0;
            value = 0;
        }
    }
};

struct node {
    enum node_type { NUMBER_NODE, OPERATOR_NODE, VARIABLE_NODE} type;
    double node_value;
    char node_operator;
    char node_variable;
    node* left_child;
    node* right_child;
    
    node(double value) : 
        type(NUMBER_NODE), node_value(value), node_operator(0), node_variable(0), left_child(nullptr), right_child(nullptr) {}
    node(char operador, node* lhs, node* rhs) : 
        type(OPERATOR_NODE), node_value(0), node_operator(operador), node_variable(0), left_child(lhs), right_child(rhs) {}
    node(char variable_name) : 
        type(VARIABLE_NODE), node_value(0), node_operator(0), node_variable(variable_name), left_child(nullptr), right_child(nullptr) {}

};

std::vector<token> tokenize(const std::string &expression){
    std::vector<token> tokens;
    std::istringstream iss(expression);
    char c;
    
    while (iss >> c){
        if (std::isdigit(c)) {
            iss.putback(c);

            double current_number;
            iss >> current_number;
            tokens.emplace_back(token(current_number));
        } else if (std::isalpha(c) || c == '+' || c == '-' || c == '*' || c == '/' || c == '(' || c == ')' || c == '^') {
            tokens.emplace_back(token(c));
        } else {
            throw std::runtime_error("Unknown Token: " + std::string(1, c));
        }
    }
    return tokens;
}

void print_token(const token &some_token){
    if(some_token.type == NUMBER){
        std::cout << "Num:" << some_token.value;
    } else if (some_token.type == OPERATOR){
        std::cout << "Op:" << some_token.token_operator << " p=" << get_precedence(some_token.token_operator);
    } else if (some_token.type == VARIABLE){
        std::cout << "Var:" << some_token.variable;
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

node* rpn_tree(const std::vector<token> &tokens){
    std::stack<node*> tree;
    std::stack<token> operators;

    for(const token& current_token : tokens) {
        if (current_token.type == NUMBER){
            tree.push(new node(current_token.value));
        } else if (current_token.type == VARIABLE){
            tree.push(new node(current_token.variable));
        } else if (current_token.type == OPERATOR) {
            if (current_token.token_operator == '('){
                operators.push(current_token);
            } else if (current_token.token_operator == ')'){
                while (!operators.empty() && operators.top().token_operator != '('){
                    char operation = operators.top().token_operator;
                    operators.pop();
                    node* right = tree.top(); tree.pop();
                    node* left = tree.top(); tree.pop();
                    tree.push(new node(operation, left, right));
                }
                if (operators.empty()){throw std::runtime_error("Syntax Error: Mismatched Closing Parenthesis");}
                operators.pop(); //Get rid of opening parenthesis
            } else {
                while (!operators.empty() &&
                    (get_precedence(operators.top().token_operator) > get_precedence(current_token.token_operator) ||
                    (get_precedence(operators.top().token_operator) == get_precedence(current_token.token_operator) &&
                    get_associativity(current_token.token_operator) == LEFT))) {
                        char operation = operators.top().token_operator;
                        operators.pop();
                        node* right = tree.top(); tree.pop();
                        node* left = tree.top(); tree.pop();
                        tree.push(new node(operation, left, right));
                }
                operators.push(current_token);
            }
        }
    }

    while(!operators.empty()){
        if (operators.top().token_operator == '(') { 
            throw std::runtime_error("Syntax Error: Mismatched Opening Parenthesis");
        }
        char operation = operators.top().token_operator;
        operators.pop();
        node* right = tree.top(); tree.pop();
        node* left = tree.top(); tree.pop();
        tree.push(new node(operation, left, right));
    }
    return tree.top();
}

void accumulate(node* n, std::unordered_map<char, int>& varCounts, double& constantSum, char oper) {
    if (n) {
        if (n->type == node::NUMBER_NODE) {
            if (oper == '+') {
                constantSum += n->node_value;
            } else if (oper == '*') {
                constantSum *= n->node_value; // Note: This may need review based on your specific use case.
            }
        } else if (n->type == node::VARIABLE_NODE) {
            varCounts[n->node_variable]++; // Increment count for the variable
        } else if (n->type == node::OPERATOR_NODE) {
            if (n->node_operator == '+') {
                accumulate(n->left_child, varCounts, constantSum, '+');
                accumulate(n->right_child, varCounts, constantSum, '+');
            } else if (n->node_operator == '*') {
                accumulate(n->left_child, varCounts, constantSum, '*');
                accumulate(n->right_child, varCounts, constantSum, '*');
            }
        }
    }
}

node* simplify(node* root) {
    if (!root) return nullptr;

    // Recursively simplify the left and right children
    root->left_child = simplify(root->left_child);
    root->right_child = simplify(root->right_child);

    if (root->type == node::OPERATOR_NODE) {
        if (root->node_operator == '+') {
            double constantSum = 0;
            std::unordered_map<char, int> variableCounts;

            // Accumulate constants and variables from both children
            accumulate(root->left_child, variableCounts, constantSum, '+');
            accumulate(root->right_child, variableCounts, constantSum, '+');

            // Create a new expression combining constants and variables
            node* combinedNode = nullptr;
            if (constantSum != 0) {
                combinedNode = new node(constantSum); // Start with the constant
            }

            for (std::unordered_map<char, int>::iterator it = variableCounts.begin(); it != variableCounts.end(); ++it) {
                char varName = it->first;
                int count = it->second;
                if (count > 1) {
                    // Create count * variable node
                    combinedNode = new node('*', new node(static_cast<double>(count)), new node(varName)); // Convert count to double
                } else {
                    if (combinedNode) {
                        combinedNode = new node('+', combinedNode, new node(varName)); // Combine with existing
                    } else {
                        combinedNode = new node(varName); // Only variables, no constant
                    }
                }
            }

            return combinedNode ? combinedNode : new node(0.0); // Return 0 if nothing
        } else if (root->node_operator == '*') {
            double constantProduct = 1;
            std::unordered_map<char, int> variableCounts;

            // Accumulate constants and variables from both children
            accumulate(root->left_child, variableCounts, constantProduct, '*');
            accumulate(root->right_child, variableCounts, constantProduct, '*');

            // Create a new expression combining constants and variables
            node* productNode = nullptr;
            if (constantProduct != 1) {
                productNode = new node(constantProduct); // Start with the constant
            }

            for (std::unordered_map<char, int>::iterator it = variableCounts.begin(); it != variableCounts.end(); ++it) {
                char varName = it->first;
                productNode = new node('*', productNode, new node(varName)); // Always multiply variables
            }

            return productNode ? productNode : new node(1.0); // Return 1 if nothing
        }
    }

    return root; // Return the root if it's not an operator
}


std::vector<token> generate_rpn_string(node* root){
    std::vector<token> rpl;

    if (root) {
        if (root->type == node::NUMBER_NODE) {
            rpl.emplace_back(token(root->node_value));
        } else if (root->type == node::VARIABLE_NODE) {
            rpl.emplace_back(token(root->node_variable));
        } else if (root->type == node::OPERATOR_NODE) {
            std::vector<token> left_rpl = generate_rpn_string(root->left_child);
            rpl.insert(rpl.end(), left_rpl.begin(), left_rpl.end());       

            std::vector<token> right_rpl = generate_rpn_string(root->right_child);
            rpl.insert(rpl.end(), right_rpl.begin(), right_rpl.end());       

            rpl.emplace_back(token(root->node_operator));
        }
    }
    return rpl;
}

std::string show_expression(const std::vector<token> &tokens) {
    std::ostringstream expression;
    for (const token &current_token : tokens) {
        if (current_token.type == NUMBER) {
            expression << current_token.value << " ";
        } else if (current_token.type == OPERATOR) {
            expression << current_token.token_operator << " ";
        } else if (current_token.type == VARIABLE) {
            expression << current_token.variable << " ";
        }
    }
    return expression.str();
}

double evaluate(const std::vector<token>& tokens){
    std::stack<double> stack;

    for(const token& current_token : tokens){
        if(current_token.type == NUMBER) {
            stack.push(current_token.value);
        } else if (current_token.type == OPERATOR){
            if (stack.size() < 2) {throw std::runtime_error("Syntax Error: Invalid Expression");}
            double rhs = stack.top(); stack.pop();
            double lhs = stack.top(); stack.pop();

            double result;
            switch (current_token.token_operator) {
                case '+': result = lhs + rhs; break;
                case '-': result = lhs - rhs; break;
                case '*': result = lhs * rhs; break;
                case '/': 
                    if(rhs == 0) {throw std::runtime_error("Math Error: Division by zero");}
                    result = lhs / rhs; break;
                case '^': result = std::pow(lhs,rhs); break;
                default: throw std::runtime_error("Unknown operator");
            }
            stack.push(result);
        }
    }
    if (stack.size() != 1){throw std::runtime_error("Syntax Error: Invalid Expression");}
    return stack.top();
}


bool contains_variables(const std::vector<token>& tokens) {
    for (const token& current_token : tokens) {
        if (current_token.type == VARIABLE) {
            return true;
        }
    }
    return false;
}

std::set<char> collect_variables(const std::vector<token> &tokens){
    std::set<char> variables;

    for (const token& current_token : tokens){
        if (current_token.type == VARIABLE){
            variables.insert(current_token.variable);
        }
    }
    return variables;
}

int main(){
    std::string input;
    printf("Expression: ");
    std::getline(std::cin, input);
    
    std::vector<token> tokens = tokenize(input);
    /*
    printf("Tokenized Input: ");
    print_tokens(tokens);
    */

    std::vector<token> output_tokens = generate_rpn_string(rpn_tree(tokens));
    std::vector<token> simplified_tokens = generate_rpn_string(simplify(rpn_tree(tokens)));
    /*
    printf("\nTokenized Output: ");
    print_tokens(output_tokens); 
    */

    printf("RPN: ");
    std::cout << show_expression(output_tokens);

    if(!contains_variables(output_tokens)){
        printf("\nResult: ");
        std::cout << evaluate(output_tokens);
    } else {
        printf("\nSimplification: ");
        std::cout << show_expression((simplified_tokens));
        printf("\nVariable(s) found: ");
        for (const char& variable : collect_variables(output_tokens)) {
            std::cout << variable << " ";
        }
    }
}


