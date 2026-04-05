#include "calc.hpp"

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

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(line[i])) {
        ++i;
    }
    return i;
}

double parse_arg(const std::string & line, std::size_t & i)
{
    double res = 0;
    std::size_t count = 0;
    bool good = true;
    bool integer = true;
    double fraction = 1;
    int base = 10;

    if (i < line.size() && line[i] == '0') {
        if (i + 1 < line.size()) {
            char next = line[i + 1];
            if (next == 'x' || next == 'X') {
                base = 16;
                i += 2;
            } else if (next == 'b' || next == 'B') {
                base = 2;
                i += 2;
            } else if (next >= '0' && next <= '9') {
                base = 8;
                i += 1;
            }
        }
    }

    while (good && i < line.size() && count < max_decimal_digits) {
        char c = line[i];
        int val = -1;

        if (c >= '0' && c <= '9') {
            val = c - '0';
        } else if (c >= 'a' && c <= 'f') {
            val = c - 'a' + 10;
        } else if (c >= 'A' && c <= 'F') {
            val = c - 'A' + 10;
        }

        if (val >= 0 && val < base) {
            if (integer) {
                res *= base;
                res += val;
            } else {
                fraction /= base;
                res += val * fraction;
            }
            ++i;
            
            if (res > 0 || !integer) {
                ++count;
            }
        } else if (c == '.') {
            integer = false;
            ++i;
        } else {
            good = false;
        }
    }

    return res;
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
    std::size_t i = 0;
    
    bool is_fold = false;
    if (i < line.size() && line[i] == '(') {
        is_fold = true;
        ++i;
    }

    const auto op = parse_op(line, i);

    if (is_fold) {
        if (i >= line.size() || line[i] != ')') {
            std::cerr << "Expected ')' after fold operation" << std::endl;
            return current;
        }
        ++i;

        if (arity(op) != 2 || op == Op::SET) {
            std::cerr << "Fold requires a binary operation" << std::endl;
            return current;
        }

        double result = current;
        bool has_args = false;

        while (true) {
            i = skip_ws(line, i);
            if (i >= line.size()) {
                break;
            }

            const auto old_i = i;
            const auto arg = parse_arg(line, i);

            if (i == old_i) {
                std::cerr << "Invalid argument in fold operation at index " << i << ": '" << line.substr(i) << "'" << std::endl;
                break; 
            }
            result = binary(op, result, arg);
            has_args = true;
        }

        if (!has_args) {
            std::cerr << "No arguments provided for fold" << std::endl;
            return current;
        }

        return result;
    }

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
                        std::cerr << "Unexpected suffix after binary operation: '" << line.substr(i) << "'" << std::endl;
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