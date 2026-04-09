#include "../include/calc.hpp"

#define _USE_MATH_DEFINES // for M_PI
#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr
#include <string> // for std::string::compare

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

    , SIN
    , COS
    , TAN
    , CTN
    , ASIN
    , ACOS
    , ATAN
    , ACTN
    , RAD
    , DEG
};

std::size_t arity(const Op op)
{
    switch (op) {
        // error
        case Op::ERR: return 0;
        // unary
        case Op::NEG: return 1;
        case Op::SQRT: return 1;

        case Op::SIN: return 1;
        case Op::COS: return 1;
        case Op::TAN: return 1;
        case Op::CTN: return 1;
        case Op::ASIN: return 1;
        case Op::ACOS: return 1;
        case Op::ATAN: return 1;
        case Op::ACTN: return 1;
        case Op::RAD: return 1;
        case Op::DEG: return 1;
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
            i--;
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
        // реализовал проверку "текстовых" операций с помощью std::string::compare
        // чтобы избавиться от большого количества влолженных свитч кейсов
        case 'A':
            if (line.compare(i, 3, "SIN") == 0) {
                i += 3;
                return Op::ASIN;
            }
            else if (line.compare(i, 3, "COS") == 0) {
                i += 3;
                return Op::ACOS;
            }
            else if (line.compare(i, 3, "TAN") == 0) {
                i += 3;
                return Op::ATAN;
            }
            else if (line.compare(i, 3, "CTN") == 0) {
                i += 3;
                return Op::ACTN;
            }
            return rollback(1);

        case 'C':
            if (line.compare(i, 2, "OS") == 0) {
                i += 2;
                return Op::COS;
            }
            else if (line.compare(i, 2, "TN") == 0) {
                i += 2;
                return Op::CTN;
            }
            return rollback(1);

        case 'D':
            if (line.compare(i, 2, "EG") == 0) {
                i += 2;
                return Op::DEG;
            }
            return rollback(1);

        case 'R':
            if (line.compare(i, 2, "AD") == 0) {
                i += 2;
                return Op::RAD;
            }
            return rollback(1);

        case 'S':
            if (line.compare(i, 3, "QRT") == 0) {
                i += 3;
                return Op::SQRT;
            }
            else if (line.compare(i, 2, "IN") == 0) {
                i += 2;
                return Op::SIN;
            }
            return rollback(1);

        case 'T':
            if (line.compare(i, 2, "AN") == 0) {
                i += 2;
                return Op::TAN;
            }
            return rollback(1);

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
            default:
                good = false;
                break;
        }
    }
    if (!good) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
    }
    else if (i < line.size()) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
    }
    return res;
}

double to_rad(const double current, bool & rad_on)
{
    if (!rad_on) {
        return current * M_PI / 180.0;
    }
    return current;

}

double to_deg(const double current, bool & rad_on)
{
    if (!rad_on) {
        return current * 180.0 / M_PI;
    }
    return current;
}

double unary(const double current, bool & rad_on, const Op op)
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
                return current; // возвращаем без изменений, чтобы код не проваливался в следующий кейс
            }

        case Op::SIN:
            return std::sin(to_rad(current, rad_on));
        case Op::COS:
            return std::cos(to_rad(current, rad_on));
        case Op::TAN:
            if (std::cos(to_rad(current, rad_on)) != 0.0) {
                return std::tan(to_rad(current, rad_on));
            }
            else {
                std::cerr << "Bad argument for TAN: " << current << std::endl;
                return current;;
            }
        case Op::CTN:
            if (std::sin(to_rad(current, rad_on)) != 0.0) {
                return std::cos(to_rad(current, rad_on)) / std::sin(to_rad(current, rad_on));
            }
            else {
                std::cerr << "Bad argument for CTN: " << current << std::endl;
                return current;;
            }
        case Op::ASIN:
            if (current >= -1 && current <= 1) {
                return to_deg(std::asin(current), rad_on);
            }
            else {
                std::cerr << "Bad argument for ASIN: " << current << std::endl;
                return current;
            }
        case Op::ACOS:
            if (current >= -1 && current <= 1) {
                return to_deg(std::acos(current), rad_on);
            }
            else {
                std::cerr << "Bad argument for ACOS: " << current << std::endl;
                return current;;
            }
        case Op::ATAN:
            return to_deg(std::atan(current), rad_on);
        case Op::ACTN:
                return to_deg(M_PI / 2 - std::atan(current), rad_on);
        case Op::RAD:
            rad_on = true;
            return current;
        case Op::DEG:
            rad_on = false;
            return current;
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

double process_line(const double current, bool & rad_on, const std::string & line)
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
                    return unary(current, rad_on, op);
                }
        default: break;
    }
    return current;
}