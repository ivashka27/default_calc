#include "calc.hpp"

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr

namespace {

const std::size_t max_decimal_digits = 10;
const double pi = std::acos(-1.0);

double rou(double x) {
    return (std::abs(x) < 1e-10) ? 0.0 : x;
}

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
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            --i; 
            return Op::SET;
        case '+': return Op::ADD;
        case '-': return Op::SUB;
        case '*': return Op::MUL;
        case '/': return Op::DIV;
        case '%': return Op::REM;
        case '_': return Op::NEG;
        case '^': return Op::POW;

        case 'A':
            if (i + 3 < line.size()) {
                if (line[i]=='S' && line[i+1]=='I' && line[i+2]=='N') { i+=3; return Op::ASIN; }
                if (line[i]=='C' && line[i+1]=='O' && line[i+2]=='S') { i+=3; return Op::ACOS; }
                if (line[i]=='T' && line[i+1]=='A' && line[i+2]=='N') { i+=3; return Op::ATAN; }
                if (line[i]=='C' && line[i+1]=='T' && line[i+2]=='N') { i+=3; return Op::ACTN; }
            }
            return rollback(1);

        case 'C':
            if (i + 1 < line.size()) {
                if (line[i]=='O' && line[i+1]=='S') { i+=2; return Op::COS; }
                if (line[i]=='T' && line[i+1]=='N') { i+=2; return Op::CTN; }
            }
            return rollback(1);

        case 'D':
            if (i + 1 < line.size()) {
                if (line[i]=='E' && line[i+1]=='G') { i+=2; return Op::DEG; }
            }
            return rollback(1);

        case 'R':
            if (i + 1 < line.size()) {
                if (line[i]=='A' && line[i+1]=='D') { i+=2; return Op::RAD; }
            }
            return rollback(1);

        case 'S':
            if (i + 2 < line.size() && line[i]=='Q' && line[i+1]=='R' && line[i+2]=='T') { i+=3; return Op::SQRT; }
            if (i + 1 < line.size() && line[i]=='I' && line[i+1]=='N') { i+=2; return Op::SIN; }
            return rollback(1);

        case 'T':
            if (i + 1 < line.size() && line[i]=='A' && line[i+1]=='N') { i+=2; return Op::TAN; }
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

double unary(const double current, bool & rad_mode, const Op op)
{
    switch (op) {
        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current > 0) return std::sqrt(current);
            std::cerr << "Bad argument for SQRT: " << current << std::endl;
            return current;
        case Op::SIN:
            return rad_mode ? rou(std::sin(current)) : rou(std::sin(current * pi / 180.0));
        case Op::COS:
            return rad_mode ? rou(std::cos(current)) : rou(std::cos(current * pi / 180.0));
        case Op::TAN:
            if (rad_mode ? rou(std::cos(current)) == 0.0 : rou(std::cos(current * pi / 180.0)) == 0.0) {
                std::cerr << "Bad argument for TAN: " << current << std::endl;
                return current;
            }
            return rad_mode ? rou(std::tan(current)) : rou(std::tan(current * pi / 180.0));
        case Op::CTN:
            if (rad_mode ? rou(std::sin(current)) == 0.0 : rou(std::sin(current * pi / 180.0)) == 0.0) {
                std::cerr << "Bad argument for CTN: " << current << std::endl;
                return current;
            }
            return rad_mode ? rou(std::cos(current) / std::sin(current)) : rou(std::cos(current * pi / 180.0) / std::sin(current * pi / 180.0));
        case Op::ASIN:
            if (std::abs(current) > 1.0) { std::cerr << "Bad argument for ASIN: " << current << std::endl; return current; }
            return rad_mode ? rou(std::asin(current)) : rou(std::asin(current) * 180.0 / pi);
        case Op::ACOS:
            if (std::abs(current) > 1.0) { std::cerr << "Bad argument for ACOS: " << current << std::endl; return current; }
            return rad_mode ? rou(std::acos(current)) : rou(std::acos(current) * 180.0 / pi);
        case Op::ATAN:
            return rad_mode ? rou(std::atan(current)) : rou(std::atan(current) * 180.0 / pi);
        case Op::ACTN:
            return rad_mode ? rou(pi / 2.0 - std::atan(current)) : rou((pi / 2.0 - std::atan(current)) * 180.0 / pi);
        case Op::RAD:
            rad_mode = true;
            return current;
        case Op::DEG:
            rad_mode = false;
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

double process_line(const double current, const std::string & line)
{
    if (!line.empty() && line[0] == '(') {
        std::size_t close_pos = line.find(')');
        if (close_pos != std::string::npos && close_pos == 2) {
            Op fold_op = Op::ERR;
            switch (line[1]) {
                case '+': fold_op = Op::ADD; break;
                case '-': fold_op = Op::SUB; break;
                case '*': fold_op = Op::MUL; break;
                case '/': fold_op = Op::DIV; break;
                case '%': fold_op = Op::REM; break;
                case '^': fold_op = Op::POW; break;
                default: break;
            }
            if (fold_op != Op::ERR) {
                std::size_t k = close_pos + 1;
                double acc = current;
                while (k < line.size()) {
                    std::size_t old_k = k;
                    k = skip_ws(line, k);
                    double val = parse_arg(line, k);
                    if (k == old_k) break;
                    acc = binary(fold_op, acc, val);
                }
                return acc;
            }
        }
    }
    static bool rad_mode = true;
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
                    return unary(current, rad_mode, op);
                }
        default: break;
    }
    return current;
}
