
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Web.UI.HtmlControls;
using System.Web.UI.WebControls.WebParts;
using System.Web;
#endregion

public partial class glfinance_report : System.Web.UI.Page
{
    string arrmachine = "";
    public glfinance_report()
    {
        
    }
   
    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "glfinance_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            lblUser.Text = UserLogin.UserName;
            Session["Customers_Company"] = labelcompany.Text;
             if (!Page.IsPostBack)
                {
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                                       
                            dateTextBox.Text = Convert.ToString( System.DateTime.Today.Date.ToShortDateString());
                            CompanyTextBox.Text = PrmComp;


                            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                            try
                            {
                                conn.Open();

                                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'glfinance_report.aspx'  ";
                                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                                DataSet ds = new DataSet();
                                da.Fill(ds);
                                foreach (DataRow dr in ds.Tables[0].Rows)
                                {
                                    headTextBox.Text = dr["field1"].ToString();
                                    dateTextBox.Text = dr["field2"].ToString();
                                    CompanyTextBox.Text = dr["field3"].ToString();
                                    acclabelTextBox.Text = dr["field4"].ToString();
                                    begsubTextBox.Text = dr["field5"].ToString();
                                    endsubTextBox.Text = dr["field6"].ToString();
                                   
                                    if (dr["chk_field1"].ToString() == "True")
                                        CheckBox1.Checked = true;
                                    else
                                        CheckBox1.Checked = false;

                                    if (dr["chk_field2"].ToString() == "True")
                                        CheckBox2.Checked = true;
                                    else
                                        CheckBox2.Checked = false;

                                    if (dr["chk_field3"].ToString() == "True")
                                        CheckBox3.Checked = true;
                                    else
                                        CheckBox3.Checked = false;

                                    if (dr["chk_field4"].ToString() == "True")
                                        CheckBox4.Checked = true;
                                    else
                                        CheckBox4.Checked = false;

                                    if (dr["chk_field5"].ToString() == "True")
                                        CheckBox5.Checked = true;
                                    else
                                        CheckBox5.Checked = false;

                                    if (dr["chk_field6"].ToString() == "True")
                                        CheckBox6.Checked = true;
                                    else
                                        CheckBox6.Checked = false;
                                }
                            }
                            catch
                            {
                                conn.Close();
                            }
                            finally
                            {
                                conn.Close();
                            }

                            if (endsubTextBox.Text == "")
                                endsubTextBox.Text = "99999999";

                            
                }

           
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
       // ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = CompanyTextBox.Text.Trim();
        

    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        int j = 0;
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            bool ischeck = ((CheckBox)row.FindControl("chk1")).Checked;
            if (ischeck)
            {
                
                
                arrmachine = arrmachine +  GridView1.Rows[i].Cells[1].Text.Trim() + ",";
                j = j + 1;                
                
            }

        }


        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "fnstat";
        ObjectDataSource2.SelectParameters["prmGetTt"].DefaultValue = "" ;
        ObjectDataSource2.SelectParameters["prmdscr"].DefaultValue = headTextBox.Text.Trim();
        ObjectDataSource2.SelectParameters["prmtrnsdt"].DefaultValue = dateTextBox.Text.Trim();
        ObjectDataSource2.SelectParameters["prmslctrp"].DefaultValue = arrmachine;
        ObjectDataSource2.SelectParameters["prmpred"].DefaultValue = periodLabel.Text.Trim();
        ObjectDataSource2.SelectParameters["prmprecls"].DefaultValue = Convert.ToString(CheckBox1.Checked);
        ObjectDataSource2.SelectParameters["prmzeroln"].DefaultValue = Convert.ToString(CheckBox2.Checked);
        ObjectDataSource2.SelectParameters["prmsuppze"].DefaultValue = Convert.ToString(CheckBox3.Checked);
        ObjectDataSource2.SelectParameters["prmact"].DefaultValue = Convert.ToString(CheckBox4.Checked);
        ObjectDataSource2.SelectParameters["prmdecimal"].DefaultValue = Convert.ToString(CheckBox5.Checked);
        ObjectDataSource2.SelectParameters["prmmulcmp"].DefaultValue = Convert.ToString(CheckBox6.Checked);
        ObjectDataSource2.SelectParameters["prmlist"].DefaultValue = CompanyTextBox.Text.Trim();
        ObjectDataSource2.SelectParameters["prmactlvl"].DefaultValue = acclabelTextBox.Text.Trim();
        ObjectDataSource2.SelectParameters["prmbesbact"].DefaultValue = begsubTextBox.Text.Trim();
        ObjectDataSource2.SelectParameters["prmendsbact"].DefaultValue = endsubTextBox.Text.Trim();
        //ObjectDataSource2.SelectParameters["prmext"].DefaultValue = arrmachine[13].ToString();


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'glfinance_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6) values ('" + UserLogin.UserName + "','glfinance_report.aspx','" + headTextBox.Text.Trim() + "','" + dateTextBox.Text.Trim() + "','" + CompanyTextBox.Text.Trim() + "','" + acclabelTextBox.Text.Trim() + "','" + begsubTextBox.Text.Trim() + "','" + endsubTextBox.Text.Trim() + "','" + Convert.ToString(CheckBox1.Checked) + "','" + Convert.ToString(CheckBox2.Checked) + "','" + Convert.ToString(CheckBox3.Checked) + "','" + Convert.ToString(CheckBox4.Checked) + "','" + Convert.ToString(CheckBox5.Checked) + "','" + Convert.ToString(CheckBox6.Checked) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + headTextBox.Text.Trim() + "', field2 = '" + dateTextBox.Text.Trim() + "', field3 = '" + CompanyTextBox.Text.Trim() + "', field4 = '" + acclabelTextBox.Text.Trim() + "', field5 = '" + begsubTextBox.Text.Trim() + "', field6 = '" + endsubTextBox.Text.Trim() + "', chk_field1 = '" + Convert.ToString(CheckBox1.Checked) + "', chk_field2 = '" + Convert.ToString(CheckBox2.Checked) + "', chk_field3 = '" + Convert.ToString(CheckBox3.Checked) + "',chk_field4 = '" + Convert.ToString(CheckBox4.Checked) + "',chk_field5 = '" + Convert.ToString(CheckBox5.Checked) + "',chk_field6 = '" + Convert.ToString(CheckBox6.Checked) + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='glfinance_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }

        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }



       
         try
         {
        OutputLabel.Visible = true;
        HyperLink1.Visible = true;

        Label path = (Label)FormView2.FindControl("fnstatLabel");       
        HyperLink1.Text = path.Text;
        HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

        if (path.Text == "")
        {
            Label1.Text = "No Csv Exists";
            //Response.Write("<script>window.location.href= 'prod_high_report.aspx'</script>");
        }
        if (path.Text != "")
        {
            //string path2 = @"/pdfs/" + path.Text;
           // Response.Redirect(path2);
        }
       }
       catch
       {

       }
        
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        
    }
    protected void company_text_change(object sender, EventArgs e)
    {
       GridView1.DataBind();
       Page_Load(sender, e);
    }
   
}
