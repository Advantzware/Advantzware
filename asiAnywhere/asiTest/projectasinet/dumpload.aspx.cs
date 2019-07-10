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
public partial class dumpload_main : System.Web.UI.Page
{
    protected void Page_Load(Object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        //SqlUsersPath.Visible = false;
        //FileUpload1.Visible = false;
        if (!Page.IsPostBack)
        {
            DumpRadio.Checked = true;
            //SecurityPath.Text="f:\\webapps\\asinet\\";
            //UserPath.Text = "f:\\webapps\\asinet\\";
        }
        if (DumpRadio.Checked)
        {
            LoadButton.Visible = false;
            DumpButton.Visible = true;
            LoadSecurityRadio.Visible = false;
            LoadUserRadio.Visible = false;
            SecurityRadio.Visible = true;
            UserRadio.Visible = true;
            MenuRadio.Visible = true;
            SqlUsersPath.Visible = false;
            FileUpload1.Visible = false;
            menuFileUpload.Visible = false;
            menupath.Visible = false;
            //SqlDataBaseFieldSet.Visible = true;
        }
        else
        {
            DumpButton.Visible = false;
            LoadButton.Visible = true;
            LoadSecurityRadio.Visible = true;
            LoadUserRadio.Visible = true;
            SecurityRadio.Visible = false;
            UserRadio.Visible = false;
            MenuRadio.Visible = true;
            SqlUsersPath.Visible = true;
            FileUpload1.Visible = true;
            menuFileUpload.Visible = true;
            menupath.Visible = true;
            // SqlDataBaseFieldSet.Visible = false;
        }



        if (Session["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"];

            UserClass.CheckLogin(Page);
            string vUserId = UserLogin.UserName;
            string vPage = "dumpload.aspx";
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
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        if (MenuRadio.Checked)
        {
            try
            {
                conn.Open();
                SqlDataAdapter da = new SqlDataAdapter("select group_id,group_id,parent,menu_label,destination,security,description,menu_id,order_num,allowedidtypes,long_desc,target_page,menu_id2 from main_menu", conn);
                DataSet ds = new DataSet();
                da.Fill(ds);


                StringBuilder str = new StringBuilder();
                str.Append("group_id,parent,menu_label,destination,security,description,menu_id,order_num,allowedidtypes,long_desc,target_page,menu_id2");
                str.AppendLine();
                int n = ds.Tables[0].Columns.Count - 1;

                for (int i = 0; i <= ds.Tables[0].Rows.Count - 1; i++)
                {
                    for (int j = 1; j <= ds.Tables[0].Columns.Count - 1; j++)
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
                    //str.AppendLine();
                    //str.Append("\r\n");

                }

                Response.Clear();
                Response.AddHeader("content-disposition",
                "attachment;filename=Menu.csv");
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

        if (SqlUserRadio.Checked)
        {
            try
            {
                conn.Open();
                SqlDataAdapter uda = new SqlDataAdapter("select UserId,UserId,Username,Password, email, GroupId,UserId2,name from user_master", conn);
                DataSet uds = new DataSet();
                uda.Fill(uds);


                StringBuilder ustr = new StringBuilder();
                ustr.Append("UserId,Username,Password, email, GroupId,UserId2,name");
                ustr.AppendLine();
                int tot = uds.Tables[0].Columns.Count - 1;

                for (int row = 0; row <= uds.Tables[0].Rows.Count - 1; row++)
                {
                    for (int col = 1; col <= uds.Tables[0].Columns.Count - 1; col++)
                    {
                        ustr.Append('"');
                        ustr.Append(uds.Tables[0].Rows[row][col].ToString());
                        ustr.Append('"');
                        for (int k = col; k < col + 1 && col < tot; k++)
                        {
                            ustr.Append(",");
                        }

                    }
                    ustr.AppendFormat(Environment.NewLine);
                }
                Response.Clear();
                Response.AddHeader("content-disposition", "attachment;filename=SqlUsers.csv");
                Response.Charset = "";
                Response.Cache.SetCacheability(HttpCacheability.NoCache);
                Response.ContentType = "application/vnd.csv";

                System.IO.StringWriter ustringwrite = new System.IO.StringWriter();
                System.Web.UI.HtmlTextWriter uhtmlwrite = new HtmlTextWriter(ustringwrite);

                Response.Write(ustr.ToString());
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



            //try
            //{
            //    conn.Open();
            //    SqlDataAdapter da = new SqlDataAdapter("select UserId,UserId,Username,Password, email, GroupId,UserId2,name from user_master", conn);
            //    DataSet ds = new DataSet();
            //    da.Fill(ds);


            //    StringBuilder str = new StringBuilder();
            //    str.Append("UserId,Username,Password, email, GroupId,UserId2,name");
            //    str.AppendLine();
            //    int n = ds.Tables[0].Columns.Count - 1;

            //    for (int i = 0; i <= ds.Tables[0].Rows.Count - 1; i++)
            //    {
            //        for (int j = 1; j <= ds.Tables[0].Columns.Count - 1; j++)
            //        {
            //            str.Append(ds.Tables[0].Rows[i][j].ToString());
            //            for (int k = j; k < j + 1 && j < n; k++)
            //            {
            //                str.Append(",");
            //            }

            //        }
            //        str.AppendFormat(Environment.NewLine);
            //        //str.AppendLine();
            //        //str.Append("\r\n");

            //    }

            //    Response.Clear();
            //    Response.AddHeader("content-disposition",
            //    "attachment;filename=Menu.csv");
            //    Response.Charset = "";
            //    Response.Cache.SetCacheability(HttpCacheability.NoCache);
            //    Response.ContentType = "application/vnd.csv";

            //    System.IO.StringWriter stringWrite = new System.IO.StringWriter();
            //    System.Web.UI.HtmlTextWriter htmlWrite = new HtmlTextWriter(stringWrite);

            //    Response.Write(str.ToString());
            //    Response.End();
            //    conn.Close();

            //    conn.Dispose();
            //}
            //catch
            //{
            //    conn.Close();
            //}
            //finally
            //{
            //    conn.Close();
            //}

        }

        if (SecurityRadio.Checked)
        {

            ObjectDataSource1.SelectParameters["prmAct1"].DefaultValue = "Security";
            ObjectDataSource1.SelectParameters["prmAct2"].DefaultValue = "Dump";
            ObjectDataSource1.SelectParameters["PrmPath"].DefaultValue = SecurityPath.Text.Trim();

        }


        if (UserRadio.Checked)
        {
            ObjectDataSource1.SelectParameters["prmAct1"].DefaultValue = "Users";
            ObjectDataSource1.SelectParameters["prmAct2"].DefaultValue = "Dump";
            ObjectDataSource1.SelectParameters["PrmPath"].DefaultValue = UserPath.Text.Trim();
        }
    }
    protected void LoadButton_Click(object sender, EventArgs e)
    {
        if (LoadRadio.Checked)
        {

            if (LoadSecurityRadio.Checked)
            {
                ObjectDataSource1.SelectParameters["prmAct1"].DefaultValue = "Security";
                ObjectDataSource1.SelectParameters["prmAct2"].DefaultValue = "Load";
                ObjectDataSource1.SelectParameters["PrmPath"].DefaultValue = SecurityPath.Text.Trim();

            }


            if (LoadUserRadio.Checked)
            {
                ObjectDataSource1.SelectParameters["prmAct1"].DefaultValue = "Users";
                ObjectDataSource1.SelectParameters["prmAct2"].DefaultValue = "Load";
                ObjectDataSource1.SelectParameters["PrmPath"].DefaultValue = UserPath.Text.Trim();
            }

            if (SqlUserRadio.Checked)
            {
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

                try
                {
                    if (FileUpload1.HasFile)
                    {
                        conn.Open();

                        FileInfo filename = new FileInfo(FileUpload1.PostedFile.FileName);
                        string csvfilepath = Server.MapPath("MyCSVFolder") + "\\" + filename.Name;
                        FileUpload1.SaveAs(csvfilepath);
                        string filepath = Server.MapPath("MyCSVFolder");
                        string cmd = "select * from [" + filename.Name + "]";
                        if (filename.Name != "SqlUsers.csv")
                        {
                            ErrLabel.Text = "Invalid File Selected.Please Select SqlUsers.csv";
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
                                        SqlCommand deletecommand = new SqlCommand("delete from user_master", conn);
                                        deletecommand.ExecuteNonQuery();

                                        bulk.DestinationTableName = "user_master";
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

            if (MenuRadio.Checked)
            {
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

                try
                {
                    if (menuFileUpload.HasFile)
                    {
                        conn.Open();

                        FileInfo filename = new FileInfo(menuFileUpload.PostedFile.FileName);
                        string csvfilepath = Server.MapPath("MyCSVFolder") + "\\" + filename.Name;
                        menuFileUpload.SaveAs(csvfilepath);
                        string filepath = Server.MapPath("MyCSVFolder");
                        string cmd = "select * from [" + filename.Name + "]";
                        if (filename.Name != "Menu.csv")
                        {
                            ErrLabel.Text = "Invalid File Selected.Please Select Menu.csv";
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
                                        SqlCommand deletecommand = new SqlCommand("delete from main_menu", conn);

                                        deletecommand.ExecuteNonQuery();

                                        bulk.DestinationTableName = "main_menu";
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
}
