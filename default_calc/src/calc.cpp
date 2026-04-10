#include "calc.hpp"

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr

const double PI = std::acos(-1.0); // создаём константу pi

namespace {

const std::size_t max_decimal_digits = 10;

// добавляем новые ключевые слова для операций
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

// указываем арность операций (триг. функции унарны)
// переключение режима тоже унарно, но оно просто будет возвращать current
std::size_t arity(const Op op)
{
    switch (op) {
        // non-arity
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

// побуквенно парсим эти операции
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
                        switch (line[i++]) {
                            case 'N':
                                return Op::SIN;
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'C':
                switch (line[i++]) {
                    case 'O':
                        switch (line[i++]) {
                            case 'S':
                                return Op::COS;
                            default:
                                return rollback(3);
                        }
                    case 'T':
                        switch (line[i++]) {
                            case 'N':
                                return Op::CTN;
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'T':
                switch (line[i++]) {
                    case 'A':
                        switch (line[i++]) {
                            case 'N':
                                return Op::TAN;
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'A':
                switch (line[i++]) {
                    case 'S':
                        switch (line[i++]) {
                            case 'I':
                                switch (line[i++]) {
                                    case 'N':
                                        return Op::ASIN;
                                    default:
                                        return rollback(4);
                                }
                            default:
                                return rollback(3);
                        }
                    case 'C':
                        switch (line[i++]) {
                            case 'O':
                                switch (line[i++]) {
                                    case 'S':
                                        return Op::ACOS;
                                    default:
                                        return rollback(4);
                                }
                            case 'T':
                                switch (line[i++]) {
                                    case 'N':
                                        return Op::ACTN;
                                    default:
                                        return rollback(4);
                                }
                            default:
                                return rollback(3);
                        }
                    case 'T':
                        switch (line[i++]) {
                            case 'A':
                                switch (line[i++]) {
                                    case 'N':
                                        return Op::ATAN;
                                    default:
                                        return rollback(4);
                                }
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'R':
                switch (line[i++]) {
                    case 'A':
                        switch (line[i++]) {
                            case 'D':
                                return Op::RAD;
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'D':
                switch (line[i++]) {
                    case 'E':
                        switch (line[i++]) {
                            case 'G':
                                return Op::DEG;
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
    // теперь читаем аргумент до пробела
    while (good && i < line.size() && count < max_decimal_digits && line[i]!=' ') {
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
    /*
    теперь эта часть не нужна
    else if (i < line.size()) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
    }
    */
    return res;
}

// реализуем функции преобразования в радианы и градусы
double torad(const double current, const bool rad_on) 
{
    if (rad_on) {
        return current;
    }
    else {
        return current * PI / 180;
    }
}

double todeg(const double current, const bool rad_on) 
{
    if (!rad_on) {
        return current;
    }
    else {
        return current * 180 / PI;
    }
}

// реализуем новые операции
// придётся передавать rad_on и сюда, чтобы его изменять
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
                return current;
            }
        // котангенса и арккотангенса нет в стандартной библиотеке, придётся выразить их через другие функции
        case Op::SIN:
            return std::sin(torad(current,rad_on));
        case Op::COS:
            return std::cos(torad(current,rad_on));
        case Op::TAN:
            if (std::cos(torad(current,rad_on)) != 0) {
                return std::tan(torad(current,rad_on));
            }
            else {
                std::cerr << "Bad argument for TAN: " << current << std::endl;
                return current;
            }
        case Op::CTN:
            if (std::sin(torad(current,rad_on)) != 0) {
                if (std::cos(torad(current,rad_on)) != 0) {
                    return 1.0/std::tan(torad(current,rad_on));
                }
                else {
                    return 0.0;
                }
            }
            else {
                std::cerr << "Bad argument for CTN: " << current << std::endl;
                return current;
            }
        case Op::ASIN:
            if (std::abs(current) <= 1) {
                return todeg(std::asin(current),rad_on);
            }
            else {
                std::cerr << "Bad argument for ASIN: " << current << std::endl;
                return current;
            }
        case Op::ACOS:
            if (std::abs(current) <= 1) {
                return todeg(std::acos(current),rad_on);
            }
            else {
                std::cerr << "Bad argument for ACOS: " << current << std::endl;
                return current;
            }
        case Op::ATAN:
            return todeg(std::atan(current),rad_on);
        case Op::ACTN:
            return todeg(PI/2 - std::atan(current),rad_on);
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

double process_line(double current, bool & rad_on, const std::string & line) // изменено по заданию
{
    std::size_t i = 0;
    const auto op = parse_op(line, i);
    switch (arity(op)) {
        case 2: {
                    i = skip_ws(line, i);
                    const auto old_i = i;
                    auto arg = parse_arg(line, i); // больше не const, будем его менять
                    if (i == old_i) {
                        std::cerr << "No argument for a binary operation" << std::endl;
                        break;
                    }
                    else {
                        current = binary(op, current, arg);
                        // будем парсить аргументы и проводить с ними бинарную операцию
                        // пока не закончится строка
                        i = skip_ws(line, i);
                        while (i < line.size()) {
                            arg = parse_arg(line, i);
                            current = binary(op, current, arg);
                            i = skip_ws(line, i);
                        }
                    }
                    // теперь обновляем current внутри функции и возвращаем то, что в итоге
                    return current;
                }
        case 1: {
                    if (i < line.size()) {
                        std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
                        break;
                    }
                    // передаём rad_on чтобы его изменять
                    return unary(current, rad_on, op);
                }
        default: break;
    }
    return current;
}