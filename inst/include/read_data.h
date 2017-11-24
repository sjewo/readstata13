/*
 * Copyright (C) 2015 Jan Marvin Garbuszus and Sebastian Jeworutzki
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef READDATA_H
#define READDATA_H

Rcpp::List read_data(FILE * file,
                     const Rcpp::IntegerVector vartype_kk,
                     const bool missing,
                     const int8_t release,
                     const uint64_t nn, uint32_t kk,
                     const Rcpp::IntegerVector vartype_sj,
                     const std::string byteorder, const bool swapit);

#endif
