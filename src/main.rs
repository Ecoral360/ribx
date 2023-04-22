#![allow(dead_code, unused_variables)]

use clap::{Args, Parser, Subcommand, ValueEnum};
use decoder::{Decoder, DEFAULT_INDENT_LEVEL, INDENT_LEVEL};

use crate::decoder::OpRibRepr;

mod decoder;
mod decoder_opt;
mod reporter;
mod data;
mod runner;

fn sym_table() {}

#[derive(ValueEnum, Clone, Debug)]
enum MetaConfig {
    /// Include all the information about the symbol in the decoded output
    All,

    /// Include the absolute position of the symbol in the ribn
    Pos,
    /// Include the relative position of the symbol in the ribn
    RelPos,

    /// Include the characters that encode the operation in the ribn
    OpChars,
    /// Include the byte value that encode the operation in the ribn
    OpBytes,

    /// Include the characters of the arguments that encode the operation in the ribn
    OpArgsChars,
    /// Include the byte value of the arguments that encode the operation in the ribn
    OpArgsBytes,
    /// Include the type of the argument of the operation (index or literal)
    OpArgType,
}

#[derive(Args, Debug)]
struct DecodeArgs {
    #[arg(short, long)]
    /// Include the symbol table in the decoded output
    sym_table: bool,

    #[arg(short = 'f', long)]
    /// The file where the decoded output will be written (default is the standard output)
    output_file: Option<String>,

    #[arg(long, default_value_t = DEFAULT_INDENT_LEVEL)]
    /// The indentation level
    indent_level: usize,

    #[arg(short, long, value_enum, group = "meta-config-group")]
    meta: Option<Vec<MetaConfig>>,

    #[arg(long, group = "meta-config-group")]
    /// Do not include any meta information in the decoded output, only keep the operations themselves
    no_meta: bool,

    ribn: String,
}

#[derive(Args, Debug)]
struct RunArgs {}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Decode the RIBN generated by the Ribbit Compiler
    Decode(DecodeArgs),
    Run(RunArgs),
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

fn decode(ribn: String, meta: bool, include_sym_table: bool) {
    let mut decoder = Decoder::new(ribn.clone());
    let (mut symbol_table, symbol_table_str) = decoder.decode_symbol_table();
    let (pc, op_vec_repr) = decoder.decode(&mut symbol_table);

    let (sym_table_str, code_str) = ribn.split_once(';').unwrap();
    println!("Ribn:\n{}", ribn);
    if include_sym_table {
        println!(
            "\n{}\n\nSymbol Table Ribn:\n{}\n",
            "-".repeat(80),
            sym_table_str
        );
        println!("Symbol Table:");
        symbol_table_str
            .iter()
            .for_each(|s| println!("{}", if s.is_empty() { "▢" } else { s }));
    }
    println!("\n{}\n\nCode Ribn:\n{}\n", "-".repeat(80), code_str);
    println!("Code:");
    OpRibRepr::vec_to_string(&op_vec_repr, meta)
        .iter()
        .for_each(|s| println!("{}", s))
}

fn run() {}

/// Extract the symbol table
///
fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Decode(DecodeArgs {
            ribn,
            indent_level,
            meta,
            no_meta,
            sym_table,
            ..
        }) => {
            unsafe {
                INDENT_LEVEL = *indent_level;
            }
            decode(ribn.clone(), !no_meta, *sym_table);
        }

        Commands::Run(_) => todo!(),
    }
}
