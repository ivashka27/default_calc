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

int digit_in_base(const char c, const unsigned base)
{
    int v = -1;
    if (c >= '0' && c <= '9') {
        v = c - '0';
    }
    else {
        const unsigned char u = static_cast<unsigned char>(c);
        const char lc = static_cast<char>(std::tolower(u));
        if (lc >= 'a' && lc <= 'f') {
            v = 10 + (lc - 'a');
        }
    }
    if (v < 0 || static_cast<unsigned>(v) >= base) {
        return -1;
    }
    return v;
}

double parse_number_in_base(const std::string & line, std::size_t & i, const unsigned base,
    std::size_t & count, bool & good)
{
    double res = 0;
    bool integer = true;
    double fraction = 1;
    while (good && i < line.size() && count < max_decimal_digits) {
        if (line[i] == '.') {
            if (!integer) {
                good = false;
                break;
            }
            integer = false;
            ++i;
            continue;
        }
        const int d = digit_in_base(line[i], base);
        if (d < 0) {
            good = false;
            break;
        }
        if (integer) {
            res = res * static_cast<double>(base) + static_cast<double>(d);
        }
        else {
            fraction /= static_cast<double>(base);
            res += static_cast<double>(d) * fraction;
        }
        ++i;
        ++count;
    }
    return res;
}

double parse_arg(const std::string & line, std::size_t & i)
{
    std::size_t count = 0;
    bool good = true;
    unsigned base = 10;
    bool radix_prefix = false;

    if (i < line.size() && line[i] == '0' && i + 1 < line.size()) {
        const char c1 = line[i + 1];
        if (c1 == 'b' || c1 == 'B') {
            base = 2;
            radix_prefix = true;
            i += 2;
        }
        else if (c1 == 'x' || c1 == 'X') {
            base = 16;
            radix_prefix = true;
            i += 2;
        }
        else if (c1 >= '0' && c1 <= '7') {
            base = 8;
        }
    }

    const double res = parse_number_in_base(line, i, base, count, good);

    if (radix_prefix && count == 0 && good) {
        good = false;
        std::cerr << "Argument parsing error at " << i << ": expected digit after radix prefix" << std::endl;
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

}

double process_line(const double current, const std::string & line)
{
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
