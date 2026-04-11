#include "calc.hpp"

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr

namespace {

const std::size_t max_decimal_digits = 10;

const double PI = std::acos(-1.0);

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
    	// режимы
    	case Op::RAD: return 0;
        case Op::DEG: return 0;
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
                    case 'I':
                    	if (line[i++] == 'N') 
                    		return Op::SIN;
                    	return rollback(3);
                    default:
                        return rollback(2);
                }
        case 'C':
            switch (line[i++]) {
                case 'O':
                    if (line[i++] == 'S')
                        return Op::COS;
                    return rollback(3);

                case 'T':
                    if (line[i++] == 'N')
                        return Op::CTN;
                    return rollback(3);

                default:
                    return rollback(2);
            }

        case 'T':
            if (line[i++] == 'A' && line[i++] == 'N')
                return Op::TAN;
            return rollback(3);

        case 'A':
            switch (line[i++]) {
                case 'S':
                    if (line[i++] == 'I' && line[i++] == 'N')
                        return Op::ASIN;
                    return rollback(4);

                case 'C':
                    switch (line[i++]) {
                        case 'O':
                            if (line[i++] == 'S')
                                return Op::ACOS;
                            return rollback(4);

                        case 'T':
                            if (line[i++] == 'N')
                                return Op::ACTN;
                            return rollback(4);

                        default:
                            return rollback(3);
                    }

                case 'T':
                    if (line[i++] == 'A' && line[i++] == 'N')
                        return Op::ATAN;
                    return rollback(4);

                default:
                    return rollback(2);
            }

        case 'R':
            if (line[i++] == 'A' && line[i++] == 'D')
                return Op::RAD;
            return rollback(3);

        case 'D':
            if (line[i++] == 'E' && line[i++] == 'G')
                return Op::DEG;
            return rollback(3);
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

double to_rad(double x)
{
    return x * PI / 180.0;
}

double to_deg(double x)
{
    return x * 180.0 / PI;
}

double parse_arg(const std::string & line, std::size_t & i)
{
    i = skip_ws(line, i);
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

double unary(const double current, const Op op, bool & rad_on)
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
        case Op::SIN:
            return std::sin(rad_on ? current : to_rad(current));
        case Op::COS:
            return std::cos(rad_on ? current : to_rad(current));
        case Op::TAN:
            if (std::cos(rad_on ? current : to_rad(current)) == 0) {
                std::cerr << "Bad argument for TAN: " << current << std::endl;
                return current;
            }
            return std::tan(rad_on ? current : to_rad(current));	
	case Op::CTN:
            if (std::sin(rad_on ? current : to_rad(current)) == 0) {
                std::cerr << "Bad argument for CTN: " << current << std::endl;
                return current;
            }
            return 1.0 / std::tan(rad_on ? current : to_rad(current));
        case Op::ASIN:
            if (std::abs(current) <= 1)
                return rad_on ? std::asin(current) : to_deg(std::asin(current));
            std::cerr << "Bad argument for ASIN: " << current << std::endl;
            return current;
        case Op::ACOS:
            if (std::abs(current) <= 1)
                return rad_on ? std::acos(current) : to_deg(std::acos(current));
            std::cerr << "Bad argument for ACOS: " << current << std::endl;
            return current;
        case Op::ATAN:
            return rad_on ? std::atan(current) : to_deg(std::atan(current));
        case Op::ACTN:
            return rad_on ? (PI / 2 - std::atan(current)) : to_deg(PI / 2 - std::atan(current));
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

double process_line(double current, bool & rad_on, const std::string & line)
{
    std::size_t i = 0;
    i = skip_ws(line, i);

    bool is_fold = false;
    Op op = Op::ERR;
    Op fold_op = Op::ERR;

    if (i < line.size() && line[i] == '(') {
        is_fold = true;
        ++i;

        fold_op = parse_op(line, i);

        if (i >= line.size() || line[i] != ')') {
            std::cerr << "Expected ')'\n";
            return current;
        }

        ++i;
        op = fold_op;
    } else {
        op = parse_op(line, i);
    }

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

        double arg = 0;

        if (!is_fold) {
            const auto old_i = i;
            arg = parse_arg(line, i);

            if (i == old_i) {
                std::cerr << "No argument for a binary operation\n";
                return current;
            }

            current = binary(op, current, arg);
            return current;
        }

        while (true) {
            i = skip_ws(line, i);

            if (i >= line.size()) {
                break;
            }

            double x = 0;
            bool integer = true;
            double fraction = 1;
            bool read = false;

            std::size_t start_i = i;

            while (i < line.size()) {
                char c = line[i];

                if (std::isdigit(c)) {
                    read = true;

                    if (integer) {
                        x = x * 10 + (c - '0');
                    } else {
                        fraction /= 10;
                        x += (c - '0') * fraction;
                    }

                    ++i;
                } else if (c == '.') {
                    integer = false;
                    ++i;
                } else if (std::isspace(c)) {
                    break;
                } else {
                    std::cerr << "Argument parsing error at " << i
                              << ": '" << line.substr(start_i) << "'\n";
                    return current;
                }
            }

            if (!read) {
                break;
            }

            current = binary(op, current, x);
        }

        return current;
    }

    case 1: {
        if (i < line.size()) {
            std::cerr << "Unexpected suffix: '" << line.substr(i) << "'\n";
            break;
        }
        return unary(current, op, rad_on);
    }

    default:
        break;
    }

    return current;
}

// При вызове parse_arg для реализации режима свертки возникали ошибки(Argument parsing error), хотя программа работала корректно. Поэтому решил сам написать парсер внутри режима свертки чтобы избежать ошибок.
