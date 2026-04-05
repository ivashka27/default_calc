#include "calc.hpp"

#include <iostream>
#include <string>

int main()
{
    double current = 0;
    for (std::string line; std::getline(std::cin, line); ) {
        bool rad_on;
        current = process_line(current, rad_on, line);
        std::cout << current << std::endl;
    }
}