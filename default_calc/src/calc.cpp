#include "calc.hpp"

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr
#include <vector>

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
    , LADD
    , LSUB
    , LMUL
    , LDIV
    , LREM
    , LPOW
    , LLOG
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
        // left fold
        case Op::LADD: return 3;
        case Op::LSUB: return 3;
        case Op::LMUL: return 3;
        case Op::LDIV: return 3;
        case Op::LREM: return 3;
        case Op::LPOW: return 3;
        case Op::LLOG: return 3;
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
        case '.':
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
            if (line.compare(i - 1, 4, "SQRT") == 0) {
                i += 3;
                return Op::SQRT;
            }
            return rollback(1);
        case 'L':
            if (line.compare(i - 1, 3, "LOG") == 0) {
                i += 2;
                return Op::LOG;
            }
            return rollback(1);
        case '(':
            if (line.compare(i - 1, 3, "(+)") == 0) {
                i += 2;
                return Op::LADD;
            }
            if (line.compare(i - 1, 3, "(-)") == 0) {
                i += 2;
                return Op::LSUB;
            }
            if (line.compare(i - 1, 3, "(*)") == 0) {
                i += 2;
                return Op::LMUL;
            }
            if (line.compare(i - 1, 3, "(/)") == 0) {
                i += 2;
                return Op::LDIV;
            }
            if (line.compare(i - 1, 3, "(%)") == 0) {
                i += 2;
                return Op::LREM;
            }
            if (line.compare(i - 1, 3, "(^)") == 0) {
                i += 2;
                return Op::LPOW;
            }
            if (line.compare(i - 1, 5, "(LOG)") == 0) {
                i += 4;
                return Op::LLOG;
            }
            return rollback(1);
        default:
            return rollback(1);
    }
}

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(static_cast<unsigned char>(line[i]))) {
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
    bool has_digit = false;
    double fraction = 1;
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
                has_digit = true;
                ++i;
                ++count;
                break;
            case '.':
                if (!integer) {
                    good = false;
                    break;
                }
                integer = false;
                ++i;
                break;
            default:
                if (std::isspace(static_cast<unsigned char>(line[i]))) {
                    return res;
                }
                good = false;
                break;
        }
    }
    if (!has_digit) {
        if (i < line.size() && !std::isspace(static_cast<unsigned char>(line[i]))) {
            std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
        }
    }
    else if (i < line.size() && std::isspace(static_cast<unsigned char>(line[i]))) {
        return res;
    }
    else if (!good) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
    }
    else if (i < line.size()) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
    }
    return res;
}

double unary(const double current, const Op op)
{
    switch (op) {
        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current >= 0) {
                return std::sqrt(current);
            }
            else {
                std::cerr << "Bad argument for SQRT: " << current << std::endl;
                return current;
            }
        case Op::FACT:
            if (current < 0 || std::floor(current) != current) {
                std::cerr << "Bad argument for factorial: " << current << std::endl;
                return current;
            }
            else {
                double result = 1;
                for (int n = 2; n <= static_cast<int>(current); ++n) {
                    result *= n;
                }
                return result;
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
            return std::log(left) / std::log(right);
        default:
            return left;
    }
}

std::vector<double> parse_args(const std::string & line, std::size_t & i)
{
    std::vector<double> numbers;

    while (i < line.size()) {
        i = skip_ws(line, i);
        if (i >= line.size()) {
            break;
        }

        const auto old_i = i;
        const auto arg = parse_arg(line, i);
        if (i == old_i) {
            return {};
        }
        else if (i < line.size() && !std::isspace(static_cast<unsigned char>(line[i]))) {
            return {};
        }
        numbers.push_back(arg);
    }

    if (numbers.empty()) {
        std::cerr << "No argument for a binary operation" << std::endl;
    }

    return numbers;
}

double left_convolution(const Op op, const double current, const std::vector<double> & numbers)
{
    double result = current;

    switch (op) {
        case Op::LADD:
            for (const auto number : numbers) {
                result += number;
            }
            return result;
        case Op::LSUB:
            for (const auto number : numbers) {
                result -= number;
            }
            return result;
        case Op::LMUL:
            for (const auto number : numbers) {
                result *= number;
            }
            return result;
        case Op::LDIV:
            for (const auto number : numbers) {
                if (number != 0) {
                    result /= number;
                }
                else {
                    std::cerr << "Bad right argument for division: " << number << std::endl;
                    return current;
                }
            }
            return result;
        case Op::LREM:
            for (const auto number : numbers) {
                if (number != 0) {
                    result = std::fmod(result, number);
                }
                else {
                    std::cerr << "Bad right argument for remainder: " << number << std::endl;
                    return current;
                }
            }
            return result;
        case Op::LPOW:
            for (const auto number : numbers) {
                result = std::pow(result, number);
            }
            return result;
        case Op::LLOG:
            for (const auto number : numbers) {
                if (result <= 0) {
                    std::cerr << "Bad argument for logarithm: " << result << std::endl;
                    return current;
                }
                else if (number <= 0 || number == 1) {
                    std::cerr << "Bad base for logarithm: " << number << std::endl;
                    return current;
                }
                result = std::log(result) / std::log(number);
            }
            return result;
        default:
            return current;
    }
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = 0;
    i = skip_ws(line, i);
    if (i >= line.size()) {
        return current;
    }

    const auto op = parse_op(line, i);
    switch (arity(op)) {
        case 3: {
                    i = skip_ws(line, i);
                    const auto numbers = parse_args(line, i);
                    if (numbers.empty()) {
                        break;
                    }
                    return left_convolution(op, current, numbers);
                }
        case 2: {
                    i = skip_ws(line, i);
                    const auto old_i = i;
                    const auto arg = parse_arg(line, i);
                    if (i == old_i) {
                        std::cerr << "No argument for a binary operation" << std::endl;
                        break;
                    }
                    i = skip_ws(line, i);
                    if (i < line.size()) {
                        break;
                    }
                    return binary(op, current, arg);
                }
        case 1: {
                    i = skip_ws(line, i);
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
