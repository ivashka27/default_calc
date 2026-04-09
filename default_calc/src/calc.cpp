#include "calc.hpp"
#include <string>
#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr

namespace {

const std::size_t max_decimal_digits = 10;

enum class Op {
      ERR
    , SET
    , ADD
    , SUB
    , MUL
    , DIV
    , REM
    , NEG
    , POW
    , SQRT
};

std::size_t arity(const Op op)
{
    switch (op) {
        // error
        case Op::ERR: return 0;
        // unary
        case Op::NEG: return 1;
        case Op::SQRT: return 1;
        // binary
        case Op::SET: return 2;
        case Op::ADD: return 2;
        case Op::SUB: return 2;
        case Op::MUL: return 2;
        case Op::DIV: return 2;
        case Op::REM: return 2;
        case Op::POW: return 2;
    }
    return 0;
}

Op parse_op(const std::string & line, std::size_t & i)
{
    const auto rollback = [&i, &line] (const std::size_t n) {
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };
    switch (line[i++]) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            --i; // a first digit is a part of op's argument
            return Op::SET;
        case '+':
            return Op::ADD;
        case '-':
            return Op::SUB;
        case '*':
            return Op::MUL;
        case '/':
            return Op::DIV;
        case '%':
            return Op::REM;
        case '_':
            return Op::NEG;
        case '^':
            return Op::POW;
        case 'S':
                switch (line[i++]) {
                    case 'Q':
                        switch (line[i++]) {
                            case 'R':
                                switch (line[i++]) {
                                    case 'T':
                                        return Op::SQRT;
                                    default:
                                        return rollback(4);
                                }
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        default:
                return rollback(1);
    }
}

double parse_number(const std::string& token) {
    if (token.size() > 2 && token[0] == '0' && (token[1] == 'b' || token[1] == 'B')) {
        std::string num = token.substr(2);
        bool frac = false;
        double result = 0.0;
        double fraction = 1.0;
        for (char c : num) {
            if (c == '.') {
                frac = true;
                continue;
            }
            int digit;
            if (c >= '0' && c <= '1') digit = c - '0';
            else return NAN;
            if (!frac) {
                result = result * 2 + digit;
            }
            else {
                fraction /= 2;
                result += digit * fraction;
            }
        }
        return result;
    }
    if (token.size() > 2 && token[0] == '0' && (token[1] == 'x' || token[1] == 'X')) {
        std::string num = token.substr(2);
        bool frac = false;
        double result = 0.0;
        double fraction = 1.0;
        for (char c : num) {
            if (c == '.') {
                frac = true;
                continue;
            }
            int digit;
            if (c >= '0' && c <= '9') digit = c - '0';
            else if (c >= 'A' && c <= 'F') digit = 10 + (c - 'A');
            else if (c >= 'a' && c <= 'f') digit = 10 + (c - 'a');
            else return NAN;
            if (!frac) {
                result = result * 16 + digit;
            }
            else {
                fraction /= 16;
                result += digit * fraction;
            }
        }
        return result;
    }
    if (token.size() > 1 && token[0] == '0' && token[1] != '.' && !(token[1] == 'b' || token[1] == 'B' || token[1] == 'x' || token[1] == 'X')) {
        std::string num = token;
        bool frac = false;
        double result = 0.0;
        double fraction = 1.0;
        for (char c : num) {
            if (c == '.') {
                frac = true;
                continue;
            }
            if (c < '0' || c > '7') return NAN;
            int digit = c - '0';
            if (!frac) {
                result = result * 8 + digit;
            }
            else {
                fraction /= 8;
                result += digit * fraction;
            }
        }
        return result;
    }
    return std::stod(token);
}

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(line[i])) {
        ++i;
    }
    return i;
}

double parse_arg(const std::string& line, std::size_t& i) {
    std::size_t start = i;
    while (i < line.size() && !std::isspace(line[i])) ++i;
    std::string token = line.substr(start, i - start);
    double val = parse_number(token);
    if (std::isnan(val)) {
        std::cerr << "Argument parsing error: " << token << std::endl;
    }
    return val;
}

double unary(const double current, const Op op)
{
    switch (op) {
        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current > 0) {
                return std::sqrt(current);
            }
            else {
                std::cerr << "Bad argument for SQRT: " << current << std::endl;
                [[fallthrough]];
            }
        default:
            return current;
    }
}

double binary(const Op op, const double left, const double right)
{
    switch (op) {
        case Op::SET:
            return right;
        case Op::ADD:
            return left + right;
        case Op::SUB:
            return left - right;
        case Op::MUL:
            return left * right;
        case Op::DIV:
            if (right != 0) {
                return left / right;
            }
            else {
                std::cerr << "Bad right argument for division: " << right << std::endl;
                return left;
            }
        case Op::REM:
            if (right != 0) {
                return std::fmod(left, right);
            }
            else {
                std::cerr << "Bad right argument for remainder: " << right << std::endl;
                return left;
            }
        case Op::POW:
            return std::pow(left, right);
        default:
            return left;
    }
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t start = skip_ws(line, 0);
    if (start >= line.size()) return current;

    if (line[start] == '(') {
        std::size_t op_start = start + 1;
        std::size_t op_end = line.find(')', op_start);
        if (op_end == std::string::npos) {
            std::cerr << "Missing closing parenthesis for fold operation" << std::endl;
            return current;
        }
        std::string op_str = line.substr(op_start, op_end - op_start);
        Op op = Op::ERR;
        if (op_str == "+") op = Op::ADD;
        else if (op_str == "-") op = Op::SUB;
        else if (op_str == "*") op = Op::MUL;
        else if (op_str == "/") op = Op::DIV;
        else if (op_str == "%") op = Op::REM;
        else if (op_str == "^") op = Op::POW;
        else {
            std::cerr << "Unknown fold operator: " << op_str << std::endl;
            return current;
        }

        std::size_t after_paren = skip_ws(line, op_end + 1);
        std::string rest = line.substr(after_paren);
        if (rest.empty()) {
            std::cerr << "No arguments for fold operation" << std::endl;
            return current;
        }

        std::vector<std::string> tokens;
        std::string token;
        for (char c : rest) {
            if (std::isspace(c)) {
                if (!token.empty()) {
                    tokens.push_back(token);
                    token.clear();
                }
            }
            else {
                token += c;
            }
        }
        if (!token.empty()) tokens.push_back(token);

        double result = current;
        for (const auto& t : tokens) {
            double val = parse_number(t);
            if (std::isnan(val)) {
                std::cerr << "Invalid number in fold: " << t << std::endl;
                return current;
            }
            result = binary(op, result, val);
        }
        return result;
    }

    std::size_t i = 0;
    const auto op = parse_op(line, i);
    switch (arity(op)) {
        case 2: {
                    i = skip_ws(line, i);
                    const auto old_i = i;
                    const auto arg = parse_arg(line, i);
                    if (i == old_i) {
                        std::cerr << "No argument for a binary operation" << std::endl;
                        break;
                    }
                    else if (i < line.size()) {
                        break;
                    }
                    return binary(op, current, arg);
                }
        case 1: {
                    if (i < line.size()) {
                        std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
                        break;
                    }
                    return unary(current, op);
                }
        default: break;
    }
    return current;
}
