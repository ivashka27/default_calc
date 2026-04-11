#include "calc.hpp"

#include <iostream>
#include <string>

int main()
{
    bool rad_on = true;
    double current = 0;
    for (std::string line; std::getline(std::cin, line); ) {
        current = process_line(current, rad_on, line);
        std::cout << current << std::endl;
    }
}
