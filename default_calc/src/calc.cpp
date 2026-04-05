#include "calc.hpp"

#include <cctype>   // for std::isspace
#include <cmath>    // various math functions
#include <iostream> // for error reporting via std::cerr
#include <vector>

namespace
{

    const double PI = std::acos(-1);
    const std::size_t max_decimal_digits = 10;

    enum class Op  // add new labels
    {
        ERR,
        SET,

        //trig
        ADD,
        SUB,
        MUL,
        DIV,
        REM,
        NEG,
        POW,
        SQRT,
        SIN,
        COS,
        TAN,
        CTN,
        ASIN,
        ACOS,
        ATAN,
        ACTN,

        RAD,
        DEG,

        // operatoin for left convolution
        LADD,
        LSUB,
        LMUL,
        LDIV,
        LREM,
        LPOW,

    };

    std::size_t arity(const Op op)  // add new returns. 3 - for angle mode change. 4 - for left convolution
    {
        switch (op)
        {
        // error
        case Op::ERR:
            return 0;
        // unary
        case Op::NEG:
            return 1;
        case Op::SQRT:
            return 1;
        case Op::SIN:
            return 1;
        case Op::COS:
            return 1;
        case Op::TAN:
            return 1;
        case Op::CTN:
            return 1;
        case Op::ASIN:
            return 1;
        case Op::ACOS:
            return 1;
        case Op::ATAN:
            return 1;
        case Op::ACTN:
            return 1;
        // binary
        case Op::SET:
            return 2;
        case Op::ADD:
            return 2;
        case Op::SUB:
            return 2;
        case Op::MUL:
            return 2;
        case Op::DIV:
            return 2;
        case Op::REM:
            return 2;
        case Op::POW:
            return 2;
        // mode switcher
        case Op::DEG:
            return 3;
        case Op::RAD:
            return 3;
        // left convolution
        case Op::LADD:
            return 4;
        case Op::LSUB:
            return 4;
        case Op::LMUL:
            return 4;
        case Op::LDIV:
            return 4;
        case Op::LREM:
            return 4;
        case Op::LPOW:
            return 4;
        }
        return 0;
    }

    Op parse_op(const std::string &line, std::size_t &i)  // add parse trig functions, convolution. 
    {
        const auto rollback = [&i, &line](const std::size_t n)
        {
            i -= n;
            std::cerr << "Unknown operation " << line << std::endl;
            return Op::ERR;
        };
        switch (line[i++])
        {
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
            switch (line[i++])
            {
            case 'Q':
                switch (line[i++])
                {
                case 'R':
                    switch (line[i++])
                    {
                    case 'T':
                        return Op::SQRT;
                    default:
                        return rollback(4);
                    }
                default:
                    return rollback(3);
                }
            // default:
            //     return rollback(2);
            case 'I':
                switch (line[i++])
                {
                case 'N':
                    return Op::SIN;
                default:
                    return rollback(3);
                }
            default:
                return rollback(2);
            }
        case 'C':
            switch (line[i++])
            {
            case 'O':
                switch (line[i++])
                {
                case 'S':
                    return Op::COS;
                default:
                    return rollback(3);
                }
            case 'T':
                switch (line[i++])
                {
                case 'N':
                    return Op::CTN;
                default:
                    return rollback(3);
                }
            default:
                return rollback(2);
            }
        case 'A':
            switch (line[i++])
            {
            case 'S':
                switch (line[i++])
                {
                case 'I':
                    switch (line[i++])
                    {
                    case 'N':
                        return Op::ASIN;
                    default:
                        return rollback(4);
                    }
                default:
                    return rollback(3);
                }
            case 'C':
                switch (line[i++])
                {
                case 'O':
                    switch (line[i++])
                    {
                    case 'S':
                        return Op::ACOS;
                    default:
                        return rollback(4);
                    }
                case 'T':
                    switch (line[i++])
                    {
                    case 'N':
                        return Op::ACTN;
                    default:
                        return rollback(4);
                    }
                default:
                    return rollback(3);
                }
            case 'T':
                switch (line[i++])
                {
                case 'A':
                    switch (line[i++])
                    {
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
            switch (line[i++])
            {
            case 'A':
                switch (line[i++])
                {
                case 'D':
                    return Op::RAD;
                default:
                    return rollback(3);
                }
            default:
                return rollback(2);
            }
        case 'D':
            switch (line[i++])
            {
            case 'E':
                switch (line[i++])
                {
                case 'G':
                    return Op::DEG;
                default:
                    return rollback(3);
                }
            default:
                return rollback(2);
            }
        case 'T':
            switch (line[i++])
            {
            case 'A':
                switch (line[i++])
                {
                case 'N':
                    return Op::TAN;
                default:
                    return rollback(3);
                }
            default:
                return rollback(2);
            }
        case '(':
            switch (line[i++])
            {
            case '+':
                switch (line[i++])
                {
                case ')':
                    return Op::LADD;
                default:
                    return rollback(3);
                }
            case '-':
                switch (line[i++])
                {
                case ')':
                    return Op::LSUB;
                default:
                    return rollback(3);
                }
            case '*':
                switch (line[i++])
                {
                case ')':
                    return Op::LMUL;
                default:
                    return rollback(3);
                }
            case '/':
                switch (line[i++])
                {
                case ')':
                    return Op::LDIV;
                default:
                    return rollback(3);
                }
            case '%':
                switch (line[i++])
                {
                case ')':
                    return Op::LREM;
                default:
                    return rollback(3);
                }
            case '_':
                switch (line[i++])
                {
                case ')':
                    return Op::LSUB;
                default:
                    return rollback(3);
                }
            case '^':
                switch (line[i++])
                {
                case ')':
                    return Op::LPOW;
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

    std::size_t skip_ws(const std::string &line, std::size_t i)
    {
        while (i < line.size() && std::isspace(line[i]))
        {
            ++i;
        }
        return i;
    }

    double parse_arg(const std::string &line, std::size_t &i) 
    /*add bool is_not_space. 
    this is implemented for   parse_many_args   funcutioin. without it  parse_arg  falls into infinite loop in case of calling parse_arg in parse_many_args
    */ 
    {
        double res = 0;
        std::size_t count = 0;
        bool good = true;
        bool is_not_space = true;
        bool integer = true;
        double fraction = 1;
        while (good && i < line.size() && count < max_decimal_digits && is_not_space)
        {
            switch (line[i])
            {
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
                if (integer)
                {
                    res *= 10;
                    res += line[i] - '0';
                }
                else
                {
                    fraction /= 10;
                    res += (line[i] - '0') * fraction;
                }
                ++i;
                ++count;
                break;
            case '.':
                integer = false;
                i++;
                break;
            case ' ':
                is_not_space = false; 
                break;
            default:
                good = false;
                break;
            }
        }
        if (!good)
        {
            std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
        }

        else if (i < line.size() && is_not_space) // add    is_not_space.  
        {
            std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
        }
        return res;
    }

    std::vector<double> parse_many_args(const std::string &line, std::size_t &i) // parsing args for left convolution. 
    {
        std::vector<double> args = {};
        while (i < line.size())
        {
            i = skip_ws(line, i);
            std::size_t old_i = i;
            double res = parse_arg(line, i);
            if (old_i == i)
            {
                std::cerr << "Arguments aren't parsed" << std::endl;
                return {};
            }
            args.push_back(res);
        }
        return args;
    }

    double unary(const double current, bool &rad_on, const Op op)
    {
        auto from_Angle_to_Rad = [&rad_on](double angle)
        {
            return rad_on ? angle : angle * (PI / 180);
        };
        auto from_Radians_to_Angle = [&rad_on](double rad)
        {
            return rad_on ? rad : rad * (180 / PI);
        };

        switch (op)
        {

        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current > 0)
            {
                return std::sqrt(current);
            }
            else
            {
                std::cerr << "Bad argument for SQRT: " << current << std::endl;
                return current;
            }
        case Op::ACOS:
            if (current <= 1 && current >= -1)
            {
                return from_Radians_to_Angle(std::acos(current));
            }
            else
            {
                std::cerr << "Bad argument for ACOS: " << current << std::endl;
                return current;
            }
        case Op::ASIN:
            if (current <= 1 && current >= -1)
            {
                return from_Radians_to_Angle(std::asin(current));
            }
            else
            {
                std::cerr << "Bad argument for ASIN: " << current << std::endl;
                return current;
            }
        case Op::ATAN:
            return from_Radians_to_Angle(std::atan(current));
        case Op::ACTN:
            return from_Radians_to_Angle((PI / 2) - std::atan(current));
        case Op::SIN:
            return (std::sin(from_Angle_to_Rad(current)));
        case Op::COS:
            return (std::cos(from_Angle_to_Rad(current)));
        case Op::TAN:
            return (std::tan(from_Angle_to_Rad(current)));
        case Op::CTN:
            return (1 / std::tan(from_Angle_to_Rad(current)));
        default:
            return current;
        }
    }

    double binary(const Op op, const double left, const double right)
    {
        switch (op)
        {
        case Op::SET:
            return right;
        case Op::ADD:
            return left + right;
        case Op::SUB:
            return left - right;
        case Op::MUL:
            return left * right;
        case Op::DIV:
            if (right != 0)
            {
                return left / right;
            }
            else
            {
                std::cerr << "Bad right argument for division: " << right << std::endl;
                return left;
            }
        case Op::REM:
            if (right != 0)
            {
                return std::fmod(left, right);
            }
            else
            {
                std::cerr << "Bad right argument for remainder: " << right << std::endl;
                return left;
            }
        case Op::POW:
            return std::pow(left, right);
        default:
            return left;
        }
    }

    bool setAngleMode(const Op op) // add setAngleMode
    {
        switch (op)
        {
        case Op::DEG:
            return false;
            break;
        case Op::RAD:
            return true;
            break;
        default:
            return true;
        }
    }

    double proceedLeftConv(const double current, std::vector<double> &args, const Op op)  // calculate convolution. 
    {
        double res = current;
        switch (op)
        {
        case Op::LADD:
        {
            for (auto i = args.begin(); i != args.end(); i++)
            {
                res = res + (*i);
            }
            return res;
        }
        case Op::LSUB:
        {
            for (auto i = args.begin(); i != args.end(); i++)
            {
                res = res - (*i);
            }
            return res;
        }
        case Op::LMUL:
        {
            for (auto i = args.begin(); i != args.end(); i++)
            {
                res = res * (*i);
            }
            return res;
        }
        case Op::LDIV:
        {
            for (auto i = args.begin(); i != args.end(); i++)
            {
                if ((*i) != 0)
                {
                    res = res / (*i);
                }
                else
                {
                    std::cerr << "Bad right argument for division: " << (*i) << std::endl;
                    return current;
                }
            }
            return res;
        }
        case Op::LREM:
        {
            for (auto i = args.begin(); i != args.end(); i++)
            {
                if ((*i) != 0)
                {
                    res = fmod(res, (*i));
                }
                else
                {
                    std::cerr << "Bad right argument for division: " << (*i) << std::endl;
                    return current;
                }
            }
            return res;
        }
        case Op::LPOW:
        {
            {
                for (auto i = args.begin(); i != args.end(); i++)
                {
                    res = std::pow(res, (*i));
                }
                return res;
            }
        }
        default:
            std::cerr << "Bad argument for convolution" << std::endl;
            return current;
        }
    }
} // anonymous namespace

double process_line(double current, bool &rad_on, const std::string &line)
{
    std::size_t i = 0;
    const auto op = parse_op(line, i);
    switch (arity(op))
    {
    case 4: // proceed convolution
    {
        i = skip_ws(line, i);
        std::vector<double> args = parse_many_args(line, i);
        return proceedLeftConv(current, args, op);
    }
    case 3: // set new mode
    {
        rad_on = setAngleMode(op);
        break;
    }
    case 2:
    {
        i = skip_ws(line, i);
        const auto old_i = i;
        const auto arg = parse_arg(line, i);
        if (i == old_i)
        {
            std::cerr << "No argument for a binary operation" << std::endl;
            break;
        }
        else if (i < line.size())
        {
            break;
        }
        return binary(op, current, arg);
    }
    case 1:
    {
        if (i < line.size())
        {
            std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
            break;
        }
        return unary(current, rad_on, op);
    }
    default:
        break;
    }
    return current;
}
