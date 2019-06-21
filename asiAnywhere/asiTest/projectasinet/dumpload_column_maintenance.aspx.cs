using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;
using System.Text;
using System.Data.OleDb;
using System.IO;
using System.Data.Common;




/// <summary>
/// Summary description for Class1
/// </summary>
public partial class dumpload_column_maint : System.Web.UI.Page
{
    protected void Page_Load(Object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
    
        if (!Page.IsPostBack)
        {
            DumpRadio.Checked = true;          
        }
        if (DumpRadio.Checked)
        {
            LoadButton.Visible = false;
            DumpButton.Visible = true;            
            columnmaintFileUpload.Visible = false;
            columnmaintpath.Visible = false;           
        }
        else
        {
            DumpButton.Visible = false;
            LoadButton.Visible = true;           
            columnmaintFileUpload.Visible = true;
            columnmaintpath.Visible = true;            
        }



        if (Session["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"];

            UserClass.CheckLogin(Page);
            string vUserId = UserLogin.UserName;
            string vPage = "dumpload_column_maintenance.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
    }

    protected void DumpButton_Click(object sender, EventArgs e)
    {
        if (ddl_program_name.SelectedValue == "1")
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

            try
            {
                conn.Open();
                SqlDataAdapter da = new SqlDataAdapter("select user_name,main_program,sub_program,col_name,col_val,seq_no,display from column_maintenance", conn);
                DataSet ds = new DataSet();
                da.Fill(ds);


                StringBuilder str = new StringBuilder();
                //str.Append("group_id,parent,menu_label,destination,security,description,menu_id,order_num,allowedidtypes,long_desc,target_page,menu_id2");
                str.Append("user_name,main_program,sub_program,col_name,col_val,seq_no,display");
                str.AppendLine();
                int n = ds.Tables[0].Columns.Count - 1;

                for (int i = 0; i <= ds.Tables[0].Rows.Count - 1; i++)
                {
                    for (int j = 0; j <= ds.Tables[0].Columns.Count - 1; j++)
                    {
                        str.Append('"');
                        str.Append(ds.Tables[0].Rows[i][j].ToString());
                        str.Append('"');
                        for (int k = j; k < j + 1 && j < n; k++)
                        {
                            str.Append(",");
                        }
                    }
                    str.AppendFormat(Environment.NewLine);

                }

                Response.Clear();
                Response.AddHeader("content-disposition",
                "attachment;filename=Column_maint.csv");
                Response.Charset = "";
                Response.Cache.SetCacheability(HttpCacheability.NoCache);
                Response.ContentType = "application/vnd.csv";

                System.IO.StringWriter stringWrite = new System.IO.StringWriter();
                System.Web.UI.HtmlTextWriter htmlWrite = new HtmlTextWriter(stringWrite);

                Response.Write(str.ToString());
                Response.End();
                conn.Close();

                conn.Dispose();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }
        }
        if (ddl_program_name.SelectedValue == "2")
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

            try
            {
                conn.Open();
                SqlDataAdapter da = new SqlDataAdapter("select main_program,main_program_val,sub_program,sub_program_val from program_master", conn);
                DataSet ds = new DataSet();
                da.Fill(ds);


                StringBuilder str = new StringBuilder();
                //str.Append("group_id,parent,menu_label,destination,security,description,menu_id,order_num,allowedidtypes,long_desc,target_page,menu_id2");
                str.Append("main_program,main_program_val,sub_program,sub_program_val");
                str.AppendLine();
                int n = ds.Tables[0].Columns.Count - 1;

                for (int i = 0; i <= ds.Tables[0].Rows.Count - 1; i++)
                {
                    for (int j = 0; j <= ds.Tables[0].Columns.Count - 1; j++)
                    {
                        str.Append('"');
                        str.Append(ds.Tables[0].Rows[i][j].ToString());
                        str.Append('"');
                        for (int k = j; k < j + 1 && j < n; k++)
                        {
                            str.Append(",");
                        }

                    }
                    str.AppendFormat(Environment.NewLine);

                }

                Response.Clear();
                Response.AddHeader("content-disposition",
                "attachment;filename=Module_maint.csv");
                Response.Charset = "";
                Response.Cache.SetCacheability(HttpCacheability.NoCache);
                Response.ContentType = "application/vnd.csv";

                System.IO.StringWriter stringWrite = new System.IO.StringWriter();
                System.Web.UI.HtmlTextWriter htmlWrite = new HtmlTextWriter(stringWrite);

                Response.Write(str.ToString());
                Response.End();
                conn.Close();

                conn.Dispose();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }
        }       
    }
    protected void LoadButton_Click(object sender, EventArgs e)
    {
        if (ddl_program_name.SelectedValue == "1")
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

            try
            {
                if (columnmaintFileUpload.HasFile)
                {
                    conn.Open();

                    FileInfo filename = new FileInfo(columnmaintFileUpload.PostedFile.FileName);
                    string csvfilepath = Server.MapPath("MyCSVFolder") + "\\" + filename.Name;
                    columnmaintFileUpload.SaveAs(csvfilepath);
                    string filepath = Server.MapPath("MyCSVFolder");
                    string cmd = "select * from [" + filename.Name + "]";
                    if (filename.Name != "Column_maint.csv")
                    {
                        ErrLabel.Text = "Invalid File Selected.Please Select Column_maint.csv";
                    }
                    else
                    {
                        string connstring = "Provider=Microsoft.Jet.OLEDB.4.0; Data Source=" + filepath + ";" + "Extended Properties='text; HDR=YES;'";
                        using (OleDbConnection olconn = new OleDbConnection(connstring))
                        {
                            OleDbCommand olcommand = new OleDbCommand("select * from [" + filename.Name + "]", olconn);
                            olconn.Open();
                            using (DbDataReader dr = olcommand.ExecuteReader())
                            {
                                using (SqlBulkCopy bulk = new SqlBulkCopy(conn))
                                {
                                    SqlCommand deletecommand = new SqlCommand("delete from column_maintenance", conn);

                                    deletecommand.ExecuteNonQuery();

                                    bulk.DestinationTableName = "column_maintenance";
                                    bulk.WriteToServer(dr);
                                }
                            }
                            ErrLabel.Text = "Records Inserted Successfully";
                        }
                        conn.Close();
                    }
                }
                else
                {
                    Response.Write("<script>alert('Please Select a file to upload')</script>");
                }
            }
            catch (Exception ex)
            {
                //Response.Write(ex.Message);
                ErrLabel.Text = "Error While Uploading" + ex.Message;
                conn.Close();
            }
        }

        if (ddl_program_name.SelectedValue == "2")
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

            try
            {
                if (columnmaintFileUpload.HasFile)
                {
                    conn.Open();

                    FileInfo filename = new FileInfo(columnmaintFileUpload.PostedFile.FileName);
                    string csvfilepath = Server.MapPath("MyCSVFolder") + "\\" + filename.Name;
                    columnmaintFileUpload.SaveAs(csvfilepath);
                    string filepath = Server.MapPath("MyCSVFolder");
                    string cmd = "select * from [" + filename.Name + "]";
                    if (filename.Name != "Module_maint.csv")
                    {
                        ErrLabel.Text = "Invalid File Selected.Please Select Module_maint.csv";
                    }
                    else
                    {
                        string connstring = "Provider=Microsoft.Jet.OLEDB.4.0; Data Source=" + filepath + ";" + "Extended Properties='text; HDR=YES;'";
                        using (OleDbConnection olconn = new OleDbConnection(connstring))
                        {
                            OleDbCommand olcommand = new OleDbCommand("select * from [" + filename.Name + "]", olconn);
                            olconn.Open();
                            using (DbDataReader dr = olcommand.ExecuteReader())
                            {
                                using (SqlBulkCopy bulk = new SqlBulkCopy(conn))
                                {
                                    SqlCommand deletecommand = new SqlCommand("delete from program_master", conn);

                                    deletecommand.ExecuteNonQuery();

                                    bulk.DestinationTableName = "program_master";
                                    bulk.WriteToServer(dr);
                                }
                            }
                            ErrLabel.Text = "Records Inserted Successfully";
                        }
                        conn.Close();
                    }
                }
                else
                {
                    Response.Write("<script>alert('Please Select a file to upload')</script>");
                }
            }
            catch (Exception ex)
            {
                //Response.Write(ex.Message);
                ErrLabel.Text = "Error While Uploading" + ex.Message;
                conn.Close();
            }
        }
    }
}
