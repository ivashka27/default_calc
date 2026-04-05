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
    , FACT
    , LOG
};

std::size_t arity(const Op op)
{
    switch (op) {
        // error
        case Op::ERR: return 0;
        // unary
        case Op::NEG: return 1;
        case Op::SQRT: return 1;
        case Op::FACT: return 1;
        // binary
        case Op::SET: return 2;
        case Op::ADD: return 2;
        case Op::SUB: return 2;
        case Op::MUL: return 2;
        case Op::DIV: return 2;
        case Op::REM: return 2;
        case Op::POW: return 2;
        case Op::LOG: return 2;
    }
    return 0;
}

bool is_foldable(const Op op)
{
    switch (op) {
        case Op::ADD:
        case Op::SUB:
        case Op::MUL:
        case Op::DIV:
        case Op::REM:
        case Op::POW:
        case Op::LOG:
            return true;
        default:
            return false;
    }
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
        case '!':
            return Op::FACT;
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
        case 'L':
            switch (line[i++]) {
            case 'O':
                    switch (line[i++]) {
                case 'G':
                            return Op::LOG;
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
    bool negative = false;
    while (good && i < line.size() && count < max_decimal_digits) {
        switch (line[i]) {
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
                if (integer) {
                    res *= 10;
                    res += line[i] - '0';
                }
                else {
                    fraction /= 10;
                    res += (line[i] - '0') * fraction;
                }
                ++i;
                ++count;
                break;
            case '.':
                integer = false;
                ++i;
                break;
            case '-':
                negative = true;
                ++i;
                break;
            default:
                if (negative) {
                    return -res;
                }
                else
                    return res;
        }
    }
    if (negative) {
        return -res;
    }
    else
        return res;
}

Op parse_op_ext(const std::string & line, std::size_t & i, bool & fold)
{
    fold = false;

    if (i < line.size() && line[i] == '(') {
        ++i;
        const auto op = parse_op(line, i);

        if (op == Op::ERR) {
            return Op::ERR;
        }

        if (i >= line.size() || line[i] != ')') {
            std::cerr << "Unknown operation " << line << std::endl;
            return Op::ERR;
        }

        ++i;

        if (!is_foldable(op)) {
            std::cerr << "Fold is supported only for binary operations" << std::endl;
            return Op::ERR;
        }

        fold = true;
        return op;
    }

    return parse_op(line, i);
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
                return current;
            }
        case Op::FACT:
            if (current >= 0 && std::floor(current) == current) {
                return std::tgamma(current + 1);
            }
            else {
                std::cerr << "Bad argument for factorial: " << current << std::endl;
                return current;
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
        case Op::LOG:
            if (left <= 0) {
                std::cerr << "Bad argument for logarithm: " << left << std::endl;
                return left;
            }
            else if (right <= 0 || right == 1) {
                std::cerr << "Bad base for logarithm: " << right << std::endl;
                return left;
            }
            else
                return std::log(left)/std::log(right);
        default:
            return left;
    }
}

double fold_binary(const Op op, const double current, const std::string & line, std::size_t i)
{
    double result = current;
    bool has_arg = false;

    while (true) {
        i = skip_ws(line, i);

        if (i >= line.size()) {
            break;
        }

        const auto old_i = i;
        const auto arg = parse_arg(line, i);

        if (i == old_i) {
            std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
            return current;
        }

        has_arg = true;
        result = binary(op, result, arg);
    }

    if (!has_arg) {
        std::cerr << "No argument for a fold operation" << std::endl;
        return current;
    }

    return result;
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = 0;
    bool fold = false;
    const auto op = parse_op_ext(line, i, fold);

    if (fold) {
        return fold_binary(op, current, line, i);
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