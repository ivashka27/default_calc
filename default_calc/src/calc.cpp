#include "calc.h"

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
        case Op::NEG : return 1;
        case Op::SQRT: return 1;
        case Op::SIN : return 1;
        case Op::COS : return 1;
        case Op::TAN : return 1;
        case Op::CTN : return 1;
        case Op::ASIN: return 1;
        case Op::ACOS: return 1;
        case Op::ATAN: return 1;
        case Op::ACTN: return 1; 

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

    Op parse_op(const std::string& line, std::size_t& i)
    {
        const auto rollback = [&i, &line](const std::size_t n) {
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

            case 'I':
                if (line[i++] == 'N') {
                    return Op::SIN;
                }
                return rollback(2);

            default:
                return rollback(1);
            }
        
        case 'C':
            switch (line[i++]) {
            case 'O':
                if (line[i++] == 'S') return Op::COS;
                return rollback(2);

            case 'T':
                if (line[i++] == 'N') return Op::CTN;
                return rollback(2);

            default:
                return rollback(1);
            }
        case 'T':
            if (line[i++] == 'A' && line[i++] == 'N') {
                return Op::TAN;
            }
            return rollback(3);
        case 'A':
            switch (line[i++]) {
            case 'S':
                if (line[i++] == 'I' && line[i++] == 'N') return Op::ASIN;
                return rollback(3);

            case 'C':
                if (line[i++] == 'O' && line[i++] == 'S') return Op::ACOS;
                if (line[i - 1] == 'T' && line[i++] == 'N') return Op::ACTN;
                return rollback(3);

            case 'T':
                if (line[i++] == 'A' && line[i++] == 'N') return Op::ATAN;
                return rollback(3);

            default:
                return rollback(1);
            }
        case 'R':
            if (line[i++] == 'A' && line[i++] == 'D') return Op::RAD;
            return rollback(3);

        case 'D':
            if (line[i++] == 'E' && line[i++] == 'G') return Op::DEG;
            return rollback(3);
        default:
            return rollback(1);
        }
    }

    std::size_t skip_ws(const std::string& line, std::size_t i)
    {
        while (i < line.size() && std::isspace(line[i])) {
            ++i;
        }
        return i;
    }

    double parse_arg(const std::string& line, std::size_t& i)
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

    double unary(const double current, const Op op, bool rad_on)
    {
        const double PI = 3.14159265358979323846;

        auto to_rad = [&](double x) {
            return rad_on ? x : x * PI / 180.0;
            };

        auto from_rad = [&](double x) {
            return rad_on ? x : x * 180.0 / PI;
            };

        double x = to_rad(current);

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
        case Op::SIN: return std::sin(x);
        case Op::COS: return std::cos(x);
        case Op::TAN: return std::tan(x);

        case Op::CTN:
            if (std::abs(std::tan(x)) < 1e-12) {
                std::cerr << "Bad argument for CTN: " << current << std::endl;
                return current;
            }
            return 1.0 / std::tan(x);

        case Op::ASIN:
            if (current < -1 || current > 1) {
                std::cerr << "Bad argument for ASIN: " << current << std::endl;
                return current;
            }
            return from_rad(std::asin(current));

        case Op::ACOS:
            if (current < -1 || current > 1) {
                std::cerr << "Bad argument for ACOS: " << current << std::endl;
                return current;
            }
            return from_rad(std::acos(current));

        case Op::ATAN:
            return from_rad(std::atan(current));

        case Op::ACTN:
            return from_rad(PI / 2 - std::atan(current));
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

double process_line(double current, bool& rad_on, const std::string& line)
{

    std::size_t i = 0;
    const auto op = parse_op(line, i);

    if (op == Op::RAD) {
        rad_on = true;
        return current;
    }

    if (op == Op::DEG) {
        rad_on = false;
        return current;
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
            std::cerr << "Unexpected suffix for a unary operation: '"
                << line.substr(i) << "'" << std::endl;
            break;
        }

        return unary(current, op, rad_on);
    }

    default:
        break;
    }

    return current;
}