#include "../include/calc.hpp"

#include <iostream>
#include <string>
#include <iomanip>

int main()
{
    double current = 0;
    bool rad_on = true;
    for (std::string line; std::getline(std::cin, line); ) {
        current = process_line(current, rad_on, line);
        std::cout << std::setprecision(9) << current << std::endl;
    }
}